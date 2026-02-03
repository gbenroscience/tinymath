/*
 * tinier.c
 *
 * An alternative performance-oriented implementation for tinymath:
 * - Function bodies are parsed once into an AST and stored in the function table.
 * - call_user_func evaluates the AST directly (no reparsing).
 * - Uses a chunked arena allocator per-function to allocate AST nodes and strings.
 *
 * Build:
 *   gcc -O2 -std=c99 -Wall -lm -o tinier tinier.c
 *
 * Notes:
 * - Function bodies are parsed as single expressions (like your demo).
 * - AST nodes allocated inside a function's arena are freed in one operation
 *   when the function is removed or when ctx is destroyed.
 * - Temporary ASTs parsed outside a function (e.g. evaluating a standalone expression)
 *   are allocated with malloc and freed individually.
 */

#define _USE_MATH_DEFINES 
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <time.h>
#include <stdarg.h>
#include "diff_module.h"
#include "parser_api.h" /* public API: mp_func, mp_context forward decls, etc. */

#define MAX_IDENT_LEN 64
#define MAX_FUNC_PARAMS 32

/* ---------------- Chunked Arena Allocator ---------------- */

typedef struct ArenaBlock {
    char *buf;
    size_t cap;
    size_t off;
    struct ArenaBlock *next;
} ArenaBlock;

typedef struct Arena {
    ArenaBlock *first;
    ArenaBlock *cur;
    size_t block_size;
} Arena;

/* create a new arena with initial block_size */
static Arena *arena_create(size_t block_size) {
    Arena *a = malloc(sizeof(Arena));
    if (!a) return NULL;
    a->block_size = block_size ? block_size : 4096;
    a->first = a->cur = NULL;
    return a;
}

static void arena_destroy(Arena *a) {
    if (!a) return;
    ArenaBlock *b = a->first;
    while (b) {
        ArenaBlock *nx = b->next;
        free(b->buf);
        free(b);
        b = nx;
    }
    free(a);
}



/* allocate a new block and append */
static ArenaBlock *arena_new_block(Arena *a, size_t needed) {
    size_t cap = a->block_size;
    if (cap < needed) cap = needed;
    ArenaBlock *b = malloc(sizeof(ArenaBlock));
    if (!b) return NULL;
    b->buf = malloc(cap);
    if (!b->buf) { free(b); return NULL; }
    b->cap = cap;
    b->off = 0;
    b->next = NULL;
    if (!a->first) a->first = a->cur = b;
    else { a->cur->next = b; a->cur = b; }
    return b;
}

/* Allocate n bytes from arena (returns pointer or NULL) */
static void *arena_alloc(Arena *a, size_t n) {
    if (!a) return NULL;
    if (!a->cur || (a->cur->cap - a->cur->off) < n) {
        if (!arena_new_block(a, n)) return NULL;
    }
    void *p = a->cur->buf + a->cur->off;
    a->cur->off += n;
    return p;
}

/* allocate and zero */
static void *arena_calloc(Arena *a, size_t n) {
    void *p = arena_alloc(a, n);
    if (p) memset(p, 0, n);
    return p;
}

/* duplicate string into arena */
static char *arena_strdup(Arena *a, const char *s) {
    size_t n = strlen(s);
    char *p = arena_alloc(a, n + 1);
    if (!p) return NULL;
    memcpy(p, s, n + 1);
    return p;
}

static char *arena_strndup(Arena *a, const char *s, size_t len) {
    char *p = arena_alloc(a, len + 1);
    if (!p) return NULL;
    memcpy(p, s, len);
    p[len] = '\0';
    return p;
}
/* scratch-arena helpers (fast short-lived allocations) */
static void arena_reset(Arena *a) {
    if (!a) return;
    ArenaBlock *b = a->first;
    while (b) {
        ArenaBlock *nx = b->next;
        free(b->buf);
        free(b);
        b = nx;
    }
    a->first = a->cur = NULL;
}

/* vsnprintf into an arena: compute length then allocate and write */

static char *arena_vprintf(Arena *a, const char *fmt, va_list ap) {
    if (!a || !fmt) return NULL;
    va_list ap2;
    va_copy(ap2, ap);
    int needed = vsnprintf(NULL, 0, fmt, ap2);
    va_end(ap2);
    if (needed < 0) return NULL;
    char *p = arena_alloc(a, (size_t)needed + 1);
    if (!p) return NULL;
    vsnprintf(p, (size_t)needed + 1, fmt, ap);
    return p;
}
static char *arena_printf(Arena *a, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    char *s = arena_vprintf(a, fmt, ap);
    va_end(ap);
    return s;
}
/* ---------------- AST & node structures ---------------- */

typedef enum { NODE_NUM, NODE_VAR, NODE_BINOP, NODE_CALL } node_type_t;

typedef struct ASTNode {
    node_type_t type;
    unsigned char from_arena; /* 1 if allocated in arena (don't free individually) */
    union {
        double num; /* NODE_NUM */
        char *ident; /* NODE_VAR */
        struct { char op; struct ASTNode *l, *r; } binop; /* NODE_BINOP */
        struct { char *name; struct ASTNode **args; int n_args; } call; /* NODE_CALL */
    };
} ASTNode;

/* ---------------- Internal variable & context types ---------------- */

/* internal variable representation */
typedef struct {
    char name[MAX_IDENT_LEN];
    double value;
    int is_const;
} mp_var;

/* Define the concrete mp_context struct (parser_api.h forward-declared it) */
struct mp_context {
    mp_var *vars;
    size_t n_vars;
    size_t vars_cap;

    mp_func *funcs; /* mp_func from parser_api.h; includes opaque root/arena pointers */
    size_t n_funcs;
    size_t funcs_cap;

    int trig_mode;       /* 0=rad,1=deg,2=grad */
    int suppress_print;  /* per-context suppression */
    Arena *scratch;   /* per-statement scratch arena for temporaries */
};
 
/* ---------------- Utilities ---------------- */

static void *xmalloc(size_t n) {
    void *p = malloc(n);
    if (!p) {
        fprintf(stderr, "Out of memory\n");
        exit(1);
    }
    return p;
}

/* string duplication helper */
static char *xstrdup_n(const char *s, size_t n) {
    char *p = malloc(n + 1);
    if (!p) return NULL;
    memcpy(p, s, n);
    p[n] = '\0';
    return p;
}

/* ---------------- Lexer / tokenizer ---------------- */

typedef enum {
    TK_END = 0, TK_NUM, TK_IDENT, TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH,
    TK_PERCENT, TK_CARET, TK_LPAREN, TK_RPAREN, TK_COMMA, TK_EQ, TK_SEMI
} tok_t;

typedef struct {
    tok_t kind;
    double num;
    char ident[MAX_IDENT_LEN];
    size_t pos;
} token_t;

typedef struct {
    const char *s;
    size_t i, len;
} lexer_t;
/* ---------------- Parser that produces AST ---------------- */

struct parser_t {
    lexer_t lx;
    token_t cur;
    mp_context *ctx;
    Arena *cur_arena; /* if set, allocate AST nodes/strings in this arena */
};
/* ---------------- AST allocation helpers (respect parser's arena) ---------------- */
 

static ASTNode *ast_node_alloc(parser_t *p);
static char *ast_strdup(parser_t *p, const char *s);
static char *ast_strndup(parser_t *p, const char *s, size_t n);


static ASTNode *ast_num_p(parser_t *p, double v) {
    ASTNode *n = ast_node_alloc(p);
    n->type = NODE_NUM;
    n->num = v;
    return n;
}

static ASTNode *ast_var_p(parser_t *p, const char *name) {
    ASTNode *n = ast_node_alloc(p);
    n->type = NODE_VAR;
    n->ident = ast_strdup(p, name);
    return n;
}

static ASTNode *ast_binop_p(parser_t *p, char op, ASTNode *l, ASTNode *r) {
    ASTNode *n = ast_node_alloc(p);
    n->type = NODE_BINOP;
    n->binop.op = op;
    n->binop.l = l;
    n->binop.r = r;
    return n;
}

static ASTNode *ast_call_n_p(parser_t *p, const char *name, ASTNode **args, int n_args) {
    ASTNode *n = ast_node_alloc(p);
    n->type = NODE_CALL;
    n->call.name = ast_strdup(p, name);
    if (n_args > 0) {
        if (p->cur_arena) {
            n->call.args = arena_alloc(p->cur_arena, sizeof(ASTNode*) * n_args);
            for (int i = 0; i < n_args; ++i) n->call.args[i] = args[i];
        } else {
            n->call.args = malloc(sizeof(ASTNode*) * n_args);
            for (int i = 0; i < n_args; ++i) n->call.args[i] = args[i];
        }
    } else {
        n->call.args = NULL;
    }
    n->call.n_args = n_args;
    return n;
}

static ASTNode *ast_node_alloc(parser_t *p) {
    if (p && p->cur_arena) {
        ASTNode *n = (ASTNode*)arena_calloc(p->cur_arena, sizeof(ASTNode));
        if (!n) return NULL;
        n->from_arena = 1;
        return n;
    } else {
        ASTNode *n = malloc(sizeof(ASTNode));
        if (!n) return NULL;
        memset(n, 0, sizeof(*n));
        n->from_arena = 0;
        return n;
    }
}

static char *ast_strdup(parser_t *p, const char *s) {
    if (p && p->cur_arena) return arena_strdup(p->cur_arena, s);
    return strdup(s);
}

static char *ast_strndup(parser_t *p, const char *s, size_t n) {
    if (p && p->cur_arena) return arena_strndup(p->cur_arena, s, n);
    return xstrdup_n(s, n);
}

/* free AST nodes only if they were allocated not-from-arena */
static void ast_free(ASTNode *n) {
    if (!n) return;
    if (n->from_arena) return; /* arena-managed: don't free here */
    switch (n->type) {
    case NODE_NUM:
        free(n);
        return;
    case NODE_VAR:
        free(n->ident);
        free(n);
        return;
    case NODE_BINOP:
        ast_free(n->binop.l);
        ast_free(n->binop.r);
        free(n);
        return;
    case NODE_CALL:
        for (int i = 0; i < n->call.n_args; ++i) ast_free(n->call.args[i]);
        free(n->call.args);
        free(n->call.name);
        free(n);
        return;
    }
}

/* Convert AST to a string (for diff compatibility and printing).
   Produces a newly-allocated string (caller must free). */
static void pbuf_cat(char **out, size_t *cap, const char *s) {
    size_t need = strlen(s);
    size_t cur = (*out) ? strlen(*out) : 0;
    if (!*out) {
        *cap = need + 128;
        *out = malloc(*cap);
        (*out)[0] = '\0';
    } else if (cur + need + 1 > *cap) {
        *cap = cur + need + 128;
        *out = realloc(*out, *cap);
    }
    strcat(*out, s);
}
/*
static void ast_to_string_rec(ASTNode *n, char **out, size_t *cap) {
    char tmp[128];
    if (!n) { pbuf_cat(out, cap, "0"); return; }
    switch (n->type) {
    case NODE_NUM:
        snprintf(tmp, sizeof(tmp), "%.17g", n->num);
        pbuf_cat(out, cap, tmp);
        break;
    case NODE_VAR:
        pbuf_cat(out, cap, n->ident ? n->ident : "");
        break;
    case NODE_BINOP:
        pbuf_cat(out, cap, "(");
        ast_to_string_rec(n->binop.l, out, cap);
        snprintf(tmp, sizeof(tmp), " %c ", n->binop.op);
        pbuf_cat(out, cap, tmp);
        ast_to_string_rec(n->binop.r, out, cap);
        pbuf_cat(out, cap, ")");
        break;
    case NODE_CALL:
        pbuf_cat(out, cap, n->call.name ? n->call.name : "");
        pbuf_cat(out, cap, "(");
        for (int i = 0; i < n->call.n_args; ++i) {
            if (i) pbuf_cat(out, cap, ",");
            ast_to_string_rec(n->call.args[i], out, cap);
        }
        pbuf_cat(out, cap, ")");
        break;
    }
}

static char *ast_to_string(ASTNode *n) {
    char *out = NULL;
    size_t cap = 0;
    ast_to_string_rec(n, &out, &cap);
    if (!out) out = strdup("");
    return out;
}
*/


static void skip_ws(lexer_t *lx) {
    for (;;) {
        while (lx->i < lx->len && isspace((unsigned char)lx->s[lx->i])) lx->i++;
        if (lx->i + 1 < lx->len && lx->s[lx->i] == '/' && lx->s[lx->i+1] == '/') {
            while (lx->i < lx->len && lx->s[lx->i] != '\n') lx->i++;
            continue;
        }
        if (lx->i + 1 < lx->len && lx->s[lx->i] == '/' && lx->s[lx->i+1] == '*') {
            lx->i += 2;
            while (lx->i + 1 < lx->len && !(lx->s[lx->i] == '*' && lx->s[lx->i+1] == '/')) lx->i++;
            if (lx->i + 1 < lx->len) lx->i += 2;
            continue;
        }
        break;
    }
}

static token_t next_token(lexer_t *lx) {
    skip_ws(lx);
    token_t t; memset(&t, 0, sizeof(t));
    t.pos = lx->i;
    if (lx->i >= lx->len) { t.kind = TK_END; return t; }
    char c = lx->s[lx->i];
    if (isdigit((unsigned char)c) || (c == '.' && lx->i + 1 < lx->len && isdigit((unsigned char)lx->s[lx->i+1]))) {
        char buf[128]; size_t j = 0;
        while (lx->i < lx->len && (isdigit((unsigned char)lx->s[lx->i]) || lx->s[lx->i]=='.' || lx->s[lx->i]=='e' || lx->s[lx->i]=='E' || lx->s[lx->i]=='+' || lx->s[lx->i]=='-')) {
            if (j < sizeof(buf)-1) buf[j++] = lx->s[lx->i];
            lx->i++;
        }
        buf[j] = '\0';
        t.kind = TK_NUM;
        t.num = strtod(buf, NULL);
        return t;
    }
    if (isalpha((unsigned char)c) || c == '_') {
        char buf[MAX_IDENT_LEN]; size_t j = 0;
        while (lx->i < lx->len && (isalnum((unsigned char)lx->s[lx->i]) || lx->s[lx->i]=='_')) {
            if (j < sizeof(buf)-1) buf[j++] = lx->s[lx->i];
            lx->i++;
        }
        buf[j] = '\0';
        t.kind = TK_IDENT;
        strncpy(t.ident, buf, MAX_IDENT_LEN-1);
        t.ident[MAX_IDENT_LEN-1] = '\0';
        return t;
    }
    lx->i++;
    switch (c) {
    case '+': t.kind = TK_PLUS; break;
    case '-': t.kind = TK_MINUS; break;
    case '*': t.kind = TK_STAR; break;
    case '/': t.kind = TK_SLASH; break;
    case '%': t.kind = TK_PERCENT; break;
    case '^': t.kind = TK_CARET; break;
    case '(': t.kind = TK_LPAREN; break;
    case ')': t.kind = TK_RPAREN; break;
    case ',': t.kind = TK_COMMA; break;
    case '=': t.kind = TK_EQ; break;
    case ';': t.kind = TK_SEMI; break;
    default: t.kind = TK_END; break;
    }
    return t;
}



static void advance(parser_t *p) { p->cur = next_token(&p->lx); }
static int accept(parser_t *p, tok_t k) { if (p->cur.kind == k) { advance(p); return 1; } return 0; }

/* Forward declarations */
static ASTNode *parse_expr_ast(parser_t *p);
static ASTNode *parse_factor_ast(parser_t *p);
static ASTNode *parse_primary_ast(parser_t *p);

/* factor: primary (^ factor)* */
static ASTNode *parse_factor_ast(parser_t *p) {
    ASTNode *n = parse_primary_ast(p);
    if (accept(p, TK_CARET)) {
        ASTNode *r = parse_factor_ast(p);
        n = ast_binop_p(p, '^', n, r);
    }
    return n;
}

/* term: factor ( (*|/|%) factor )* */
static ASTNode *parse_term_ast(parser_t *p) {
    ASTNode *n = parse_factor_ast(p);
    while (p->cur.kind == TK_STAR || p->cur.kind == TK_SLASH || p->cur.kind == TK_PERCENT) {
        char op = (p->cur.kind == TK_STAR) ? '*' : (p->cur.kind == TK_SLASH) ? '/' : '%';
        advance(p);
        ASTNode *r = parse_factor_ast(p);
        n = ast_binop_p(p, op, n, r);
    }
    return n;
}

/* expr: term ( (+|-) term )* */
static ASTNode *parse_expr_ast(parser_t *p) {
    ASTNode *n = parse_term_ast(p);
    while (p->cur.kind == TK_PLUS || p->cur.kind == TK_MINUS) {
        char op = (p->cur.kind == TK_PLUS) ? '+' : '-';
        advance(p);
        ASTNode *r = parse_term_ast(p);
        n = ast_binop_p(p, op, n, r);
    }
    return n;
}

static ASTNode *parse_primary_ast(parser_t *p) {
    if (p->cur.kind == TK_NUM) {
        ASTNode *n = ast_num_p(p, p->cur.num);
        advance(p);
        return n;
    }
    if (p->cur.kind == TK_IDENT) {
        char name[MAX_IDENT_LEN];
        strncpy(name, p->cur.ident, MAX_IDENT_LEN-1);
        name[MAX_IDENT_LEN-1] = '\0';
        advance(p);

        /* function call */
        if (accept(p, TK_LPAREN)) {
            ASTNode **args = NULL;
            int n_args = 0;
            if (!accept(p, TK_RPAREN)) {
                do {
                    ASTNode *arg = parse_expr_ast(p);
                    ASTNode **tmp = realloc(args, sizeof(ASTNode*) * (n_args + 1));
                    if (!tmp) { for (int i = 0; i < n_args; ++i) ast_free(args[i]); free(args); return NULL; }
                    args = tmp;
                    args[n_args++] = arg;
                } while (accept(p, TK_COMMA));
                if (!accept(p, TK_RPAREN)) {
                    for (int i = 0; i < n_args; ++i) ast_free(args[i]);
                    free(args);
                    return NULL;
                }
            }
            ASTNode *call = ast_call_n_p(p, name, args, n_args);
            free(args); /* call copied pointers into its own array (arena or malloc) */
            return call;
        }

        /* variable */
        return ast_var_p(p, name);
    }

    if (accept(p, TK_LPAREN)) {
        ASTNode *e = parse_expr_ast(p);
        accept(p, TK_RPAREN);
        return e;
    }

    if (accept(p, TK_MINUS)) {
        ASTNode *r = parse_primary_ast(p);
        ASTNode *zero = ast_num_p(p, 0.0);
        return ast_binop_p(p, '-', zero, r);
    }
    if (accept(p, TK_PLUS)) {
        return parse_primary_ast(p);
    }

    /* unexpected token -> consume and return NaN node */
    advance(p);
    return ast_num_p(p, NAN);
}

/* ---------------- Evaluator ---------------- */

static double to_radians_ctx(mp_context *ctx, double angle) {
    if (!ctx) return angle;
    if (ctx->trig_mode == 1) return angle * M_PI / 180.0;
    if (ctx->trig_mode == 2) return angle * M_PI / 200.0;
        return angle;
}

/* builtins: return NAN if not matched */
static double call_builtin(const char *name, double *args, int n, mp_context *ctx) {
    (void)ctx;
    if (strcmp(name, "sin") == 0 && n == 1) return sin(to_radians_ctx(ctx, args[0]));
    if (strcmp(name, "cos") == 0 && n == 1) return cos(to_radians_ctx(ctx, args[0]));
    if (strcmp(name, "tan") == 0 && n == 1) return tan(to_radians_ctx(ctx, args[0]));
    if (strcmp(name, "sqrt") == 0 && n == 1) return sqrt(args[0]);
    if (strcmp(name, "log") == 0 && n == 1) return log(args[0]);
    if (strcmp(name, "exp") == 0 && n == 1) return exp(args[0]);
    if (strcmp(name, "abs") == 0 && n == 1) return fabs(args[0]);
    if (strcmp(name, "pow") == 0 && n == 2) return pow(args[0], args[1]);
    if (strcmp(name, "sum") == 0) { double s=0; for (int i=0;i<n;++i) s+=args[i]; return s; }
    /* add more as needed */
    return NAN;
}

/* Evaluate AST recursively. Returns NAN on error. */
static double eval_ast(ASTNode *n, mp_context *ctx);






// Sorting helper
static int double_cmp(const void *aa, const void *bb)
{
    double a = *(const double *)aa;
    double b = *(const double *)bb;

    int a_nan = isnan(a);
    int b_nan = isnan(b);

    if (a_nan || b_nan)
    {
        if (a_nan && b_nan)
            return 0;
        if (a_nan)
            return 1;
        return -1;
    }

    return (a > b) - (a < b);
}
// Statistical functions
static double call_stat(const char *name, double *args, int n_args)
{
    if (n_args <= 0)
        return NAN;

    double min_v = args[0], max_v = args[0];
    double sum = 0, prod = 1, sum_sq = 0;

    for (int i = 0; i < n_args; i++)
    {
        double v = args[i];
        if (v < min_v)
            min_v = v;
        if (v > max_v)
            max_v = v;
        sum += v;
        prod *= v;
        sum_sq += v * v;
    }
    double mean = sum / n_args;

    double ssd = 0;
    for (int i = 0; i < n_args; i++)
    {
        double diff = args[i] - mean;
        ssd += diff * diff;
    }

    double var_pop = ssd / n_args;
    double var_sam = (n_args > 1) ? ssd / (n_args - 1) : 0.0;
    double std_sam = sqrt(var_sam);
    double std_pop = sqrt(var_pop);

    if (strcmp(name, "zscore") == 0)
    {
        if (std_pop == 0)
            return 0.0;
        return (args[0] - mean) / std_pop;
    }

    /* Coefficient of variation (CV): population std dev / |mean|
       Recognize both "cov" and "cv" as synonyms here. */
    if (strcmp(name, "cov") == 0 || strcmp(name, "cv") == 0)
    {
        if (mean == 0.0)
            return NAN;
        return std_pop / fabs(mean);
    }

    double median = NAN, mode_val = NAN;
    if (strcmp(name, "median") == 0 || strcmp(name, "med") == 0 || strcmp(name, "mode") == 0)
    {
        double *sorted = malloc(n_args * sizeof(double));
        if (sorted)
        {
            memcpy(sorted, args, n_args * sizeof(double));
            qsort(sorted, n_args, sizeof(double), double_cmp);
            median = (n_args % 2 != 0) ? sorted[n_args / 2] : (sorted[n_args / 2 - 1] + sorted[n_args / 2]) / 2.0;

            mode_val = sorted[0];
            int max_c = 1, cur_c = 1;
            for (int i = 1; i < n_args; i++)
            {
                if (sorted[i] == sorted[i - 1])
                    cur_c++;
                else
                {
                    if (cur_c > max_c)
                    {
                        max_c = cur_c;
                        mode_val = sorted[i - 1];
                    }
                    cur_c = 1;
                }
            }
            if (cur_c > max_c)
                mode_val = sorted[n_args - 1];
            free(sorted);
        }
    }

    if (strcmp(name, "min") == 0)
        return min_v;
    if (strcmp(name, "max") == 0)
        return max_v;
    if (strcmp(name, "sum") == 0)
        return sum;
    if (strcmp(name, "mean") == 0 || strcmp(name, "avg") == 0)
        return mean;
    if (strcmp(name, "rng") == 0)
        return max_v - min_v;
    if (strcmp(name, "mrng") == 0)
        return (min_v + max_v) / 2.0;
    if (strcmp(name, "std_err") == 0 || strcmp(name, "sem") == 0)
        return std_sam / sqrt((double)n_args);
    if (strcmp(name, "var") == 0 || strcmp(name, "pvar") == 0)
        return var_pop;
    if (strcmp(name, "std") == 0 || strcmp(name, "pstd") == 0)
        return sqrt(var_pop);
    if (strcmp(name, "svar") == 0)
        return var_sam;
    if (strcmp(name, "sstd") == 0)
        return std_sam;
    if (strcmp(name, "median") == 0 || strcmp(name, "med") == 0)
        return median;
    if (strcmp(name, "mode") == 0)
        return mode_val;
    if (strcmp(name, "rms") == 0)
        return sqrt(sum_sq / n_args);

    return NAN;
}


/* Evaluate AST recursively. Returns NAN on error. */
static double eval_ast(ASTNode *n, mp_context *ctx);

/* Evaluate call node: builtins first, then user-defined (using stored AST) */
static double eval_call(ASTNode *node, mp_context *ctx) {
    if (!node || node->type != NODE_CALL) return NAN;

    int n_args = node->call.n_args;

    /* Small-buffer optimization for argument values: use stack array for the
       common case n_args <= MAX_FUNC_PARAMS; otherwise malloc. */
    double argvals_stack[MAX_FUNC_PARAMS];
    double *argvals = NULL;
    int argvals_heap = 0;

    if (n_args > 0) {
        if (n_args <= MAX_FUNC_PARAMS) {
            argvals = argvals_stack;
        } else {
            argvals = malloc(sizeof(double) * (size_t)n_args);
            if (!argvals) {
                fprintf(stderr, "Memory allocation failed for call args\n");
                return NAN;
            }
            argvals_heap = 1;
        }
    }

    for (int i = 0; i < n_args; ++i) {
        argvals[i] = eval_ast(node->call.args[i], ctx);
    }

    /* Zero-arg trig commands and MODE handling */
    if (n_args == 0 && strcmp(node->call.name, "DEG") == 0) {
        ctx->trig_mode = 1;
        if (argvals_heap) free(argvals);
        return NAN;
    }
    if (n_args == 0 && strcmp(node->call.name, "RAD") == 0) {
        ctx->trig_mode = 0;
        if (argvals_heap) free(argvals);
        return NAN;
    }
    if (n_args == 0 && strcmp(node->call.name, "GRAD") == 0) {
        ctx->trig_mode = 2;
        if (argvals_heap) free(argvals);
        return NAN;
    }
    if (n_args == 0 && strcmp(node->call.name, "MODE") == 0) {
        const char *m = ctx->trig_mode == 1 ? "Degrees" : ctx->trig_mode == 2 ? "Gradians" : "Radians";
        printf("Current trigonometric mode: %s\n", m);
        if (argvals_heap) free(argvals);
        return NAN;
    }

    /* Try builtin */
    double val = call_builtin(node->call.name, argvals, n_args, ctx);
    if (!isnan(val)) { if (argvals_heap) free(argvals); return val; }

    /* Try statistical functions (variable arity) */
    val = call_stat(node->call.name, argvals, n_args);
    if (!isnan(val)) { if (argvals_heap) free(argvals); return val; }

    /* Try user-defined function */
    mp_func *f = lookup_func(ctx, node->call.name);
    if (!f) { if (argvals_heap) free(argvals); return NAN; }

    if (n_args != f->n_params) {
        fprintf(stderr, "Wrong number of arguments for %s (got %d, expected %d)\n", f->name, n_args, f->n_params);
        if (argvals_heap) free(argvals);
        return NAN;
    }

    /* Save old var values / set parameters (stack allocation when small) */
    int use_stack = (f->n_params <= MAX_FUNC_PARAMS);
    double stack_oldvals[MAX_FUNC_PARAMS];
    char stack_had_old[MAX_FUNC_PARAMS];
    double *oldvals = NULL;
    char *had_old = NULL;

    if (use_stack) {
        oldvals = stack_oldvals;
        had_old = stack_had_old;
    } else {
        oldvals = malloc(sizeof(double) * (size_t)f->n_params);
        had_old = malloc(sizeof(char) * (size_t)f->n_params);
        if (!oldvals || !had_old) {
            fprintf(stderr, "Memory allocation failed in eval_call\n");
            if (!use_stack) { free(oldvals); free(had_old); }
            if (argvals_heap) free(argvals);
            return NAN;
        }
    }

    for (int i = 0; i < f->n_params; ++i) {
        double ov;
        int found = 0;
        /* lookup_var implementation below; replicate here via function */
        for (size_t vi = 0; vi < ctx->n_vars; ++vi) {
            if (strcmp(ctx->vars[vi].name, f->params[i]) == 0) { ov = ctx->vars[vi].value; found = 1; break; }
        }
        had_old[i] = found ? 1 : 0;
        if (found) oldvals[i] = ov;
        /* set_var: implement here to add/update variable */
        int updated = 0;
        for (size_t vi = 0; vi < ctx->n_vars; ++vi) {
            if (strcmp(ctx->vars[vi].name, f->params[i]) == 0) {
                if (ctx->vars[vi].is_const) {
                    fprintf(stderr, "Error: cannot assign to constant %s\n", f->params[i]);
                } else {
                    ctx->vars[vi].value = argvals[i];
                    updated = 1;
    }
                break;
            }
        }
        if (!updated) {
            if (ctx->n_vars >= ctx->vars_cap) {
                size_t newcap = ctx->vars_cap ? ctx->vars_cap * 2 : 64;
                mp_var *tmp = realloc(ctx->vars, newcap * sizeof(mp_var));
                if (!tmp) { fprintf(stderr, "Memory allocation failed while setting param\n"); break; }
                ctx->vars = tmp; ctx->vars_cap = newcap;
            }
            strncpy(ctx->vars[ctx->n_vars].name, f->params[i], MAX_IDENT_LEN-1);
            ctx->vars[ctx->n_vars].name[MAX_IDENT_LEN-1] = '\0';
            ctx->vars[ctx->n_vars].value = argvals[i];
            ctx->vars[ctx->n_vars].is_const = 0;
            ctx->n_vars++;
        }
    }

    /* Evaluate the function body AST (f->root is opaque ASTNode* from header) */
    double result = NAN;
    if (f->root) {
        result = eval_ast((ASTNode*)f->root, ctx);
    } else {
        /* fallback: parse body each time (rare if define_func populated root) */
        if (f->body) {
            parser_t sub;
            sub.lx.s = f->body;
            sub.lx.i = 0;
            sub.lx.len = strlen(f->body);
            sub.ctx = ctx;
            sub.cur_arena = NULL;
            advance(&sub);
            ASTNode *expr = parse_expr_ast(&sub);
            result = eval_ast(expr, ctx);
            ast_free(expr);
        } else {
            fprintf(stderr, "Function has no body: %s\n", f->name);
            result = NAN;
        }
    }

    /* Restore old values or remove newly added params */
    for (int i = 0; i < f->n_params; ++i) {
        if (had_old[i]) {
            /* set back to old value */
            int updated = 0;
            for (size_t vi = 0; vi < ctx->n_vars; ++vi) {
                if (strcmp(ctx->vars[vi].name, f->params[i]) == 0) {
                    ctx->vars[vi].value = oldvals[i];
                    updated = 1;
                    break;
                }
            }
            if (!updated) {
                /* shouldn't happen, but fall back to adding */
                if (ctx->n_vars >= ctx->vars_cap) {
                    size_t newcap = ctx->vars_cap ? ctx->vars_cap * 2 : 64;
                    mp_var *tmp = realloc(ctx->vars, newcap * sizeof(mp_var));
                    if (tmp) { ctx->vars = tmp; ctx->vars_cap = newcap; }
                }
                strncpy(ctx->vars[ctx->n_vars].name, f->params[i], MAX_IDENT_LEN-1);
                ctx->vars[ctx->n_vars].name[MAX_IDENT_LEN-1] = '\0';
                ctx->vars[ctx->n_vars].value = oldvals[i];
                ctx->vars[ctx->n_vars].is_const = 0;
                ctx->n_vars++;
            }
        } else {
            /* remove param we just added: linear remove */
            for (size_t j = 0; j < ctx->n_vars; ++j) {
                if (strcmp(ctx->vars[j].name, f->params[i]) == 0) {
                    for (size_t k = j + 1; k < ctx->n_vars; ++k) ctx->vars[k-1] = ctx->vars[k];
                    ctx->n_vars--;
                    break;
                }
            }
        }
    }

    if (!use_stack) { free(oldvals); free(had_old); }
    if (argvals_heap) free(argvals);
    return result;
}

static double eval_ast(ASTNode *n, mp_context *ctx) {
    if (!n) return NAN;
    switch (n->type) {
    case NODE_NUM: return n->num;
    case NODE_VAR: {
        for (size_t i = 0; i < ctx->n_vars; ++i) {
            if (strcmp(ctx->vars[i].name, n->ident) == 0) return ctx->vars[i].value;
        }
        fprintf(stderr, "Unknown variable: %s\n", n->ident);
        return NAN;
    }
    case NODE_BINOP: {
        double l = eval_ast(n->binop.l, ctx);
        double r = eval_ast(n->binop.r, ctx);
        switch (n->binop.op) {
        case '+': return l + r;
        case '-': return l - r;
        case '*': return l * r;
        case '/': return l / r;
        case '^': return pow(l, r);
        case '%': return fmod(l, r);
        default: return NAN;
        }
    }
    case NODE_CALL:
        return eval_call(n, ctx);
    default:
        return NAN;
    }
}

/* ---------------- define_func: parse body into AST and store (arena-backed) */

/* Public API: define_func declared in parser_api.h */
int define_func(mp_context *ctx, const char *name, const char params[][MAX_IDENT_LEN], int n_params, const char *body_start, size_t body_len) {
    if (!ctx) return 0;
    if (n_params > MAX_FUNC_PARAMS) {
        fprintf(stderr, "Too many parameters (max %d)\n", MAX_FUNC_PARAMS);
        return 0;
    }

    if (ctx->n_funcs >= ctx->funcs_cap) {
        size_t nc = ctx->funcs_cap ? ctx->funcs_cap * 2 : 16;
        mp_func *tmp = realloc(ctx->funcs, nc * sizeof(mp_func));
        if (!tmp) { fprintf(stderr, "Memory allocation failed for function table\n"); return 0; }
        ctx->funcs = tmp;
        ctx->funcs_cap = nc;
    }

    mp_func *f = &ctx->funcs[ctx->n_funcs++];
    memset(f, 0, sizeof(*f));
    strncpy(f->name, name, MAX_IDENT_LEN-1);
    f->name[MAX_IDENT_LEN-1] = '\0';
    f->n_params = n_params;
    for (int i = 0; i < n_params; ++i) {
        strncpy(f->params[i], params[i], MAX_IDENT_LEN-1);
        f->params[i][MAX_IDENT_LEN-1] = '\0';
    }

    /* copy body source for debugging / printing (public field) */
    f->body = xstrdup_n(body_start, body_len);

    /* create arena sized to body_len*2 (heuristic) and attach to mp_func */
    size_t blk = 4096;
    if (body_len > 4096) blk = body_len * 2;
    f->arena = arena_create(blk);
    if (!f->arena) { free(f->body); f->body = NULL; return 0; }

    /* parse AST into arena: set parser.cur_arena = f->arena */
    parser_t parser;
    memset(&parser, 0, sizeof(parser));
    parser.lx.s = f->body;
    parser.lx.i = 0;
    parser.lx.len = strlen(f->body);
    parser.ctx = ctx;
    parser.cur_arena = f->arena;
    advance(&parser); /* load first token */
    ASTNode *root = parse_expr_ast(&parser);
    f->root = root;   /* store parsed AST root in public mp_func */

    return 1;
}

/* ---------------- Parsing top-level statements (assignment, def, expr) */

typedef enum { STMT_VALUE, STMT_DEFINITION, STMT_ERROR } StmtKind;
typedef struct { StmtKind kind; double value; } StmtResult;

static int parse_func_def_and_store(parser_t *p, const char *fname) {
    if (!accept(p, TK_LPAREN)) { fprintf(stderr, "Error: Expected '(' after function name '%s'\n", fname); return 0; }
    char params[MAX_FUNC_PARAMS][MAX_IDENT_LEN];
    int n_params = 0;
    while (p->cur.kind == TK_IDENT) {
        if (n_params >= MAX_FUNC_PARAMS) { fprintf(stderr, "Too many params\n"); return 0; }
        strncpy(params[n_params], p->cur.ident, MAX_IDENT_LEN-1);
        params[n_params][MAX_IDENT_LEN-1] = '\0';
        n_params++;
        advance(p);
        if (p->cur.kind == TK_COMMA) { advance(p); if (p->cur.kind != TK_IDENT) { fprintf(stderr, "Trailing comma\n"); return 0; } }
        else break;
            }
    if (!accept(p, TK_RPAREN)) { fprintf(stderr, "Error: Expected ')' after parameters\n"); return 0; }
    if (!accept(p, TK_EQ)) { fprintf(stderr, "Error: Expected '=' to define function\n"); return 0; }

    size_t body_start_idx = p->cur.pos;
    while (p->cur.kind != TK_SEMI && p->cur.kind != TK_END) advance(p);
    size_t body_end_idx = p->cur.pos;
    size_t body_len = 0;
    if (body_end_idx > body_start_idx) body_len = body_end_idx - body_start_idx;
    if (p->cur.kind == TK_SEMI) advance(p);

    return define_func(p->ctx, fname, params, n_params, p->lx.s + body_start_idx, body_len);
}

static StmtResult parse_statement(parser_t *p) {
    StmtResult res = { STMT_ERROR, NAN };
    if (p->cur.kind == TK_IDENT) {
        char name[MAX_IDENT_LEN];
        strncpy(name, p->cur.ident, MAX_IDENT_LEN-1);
        name[MAX_IDENT_LEN-1] = '\0';

        size_t save_i = p->lx.i;
        token_t save_cur = p->cur;
        advance(p);

        if (p->cur.kind == TK_EQ) {
            advance(p);
            /* parse RHS with real parser but disable arena allocation temporarily */
            Arena *saved_arena = p->cur_arena;
            p->cur_arena = NULL;
            ASTNode *expr = parse_expr_ast(p);
            p->cur_arena = saved_arena;
            double val = eval_ast(expr, p->ctx);
            ast_free(expr);
            if (!isnan(val)) {
                /* set_var: add/update variable */
                int updated = 0;
                for (size_t vi = 0; vi < p->ctx->n_vars; ++vi) {
                    if (strcmp(p->ctx->vars[vi].name, name) == 0) {
                        if (p->ctx->vars[vi].is_const) {
                            fprintf(stderr, "Error: cannot assign to constant %s\n", name);
                        } else {
                            p->ctx->vars[vi].value = val;
                            updated = 1;
                        }
                        break;
                    }
                }
                if (!updated) {
                    if (p->ctx->n_vars >= p->ctx->vars_cap) {
                        size_t newcap = p->ctx->vars_cap ? p->ctx->vars_cap * 2 : 64;
                        mp_var *tmp = realloc(p->ctx->vars, newcap * sizeof(mp_var));
                        if (!tmp) { fprintf(stderr, "Memory allocation failed in set_var\n"); return res; }
                        p->ctx->vars = tmp; p->ctx->vars_cap = newcap;
                    }
                    strncpy(p->ctx->vars[p->ctx->n_vars].name, name, MAX_IDENT_LEN-1);
                    p->ctx->vars[p->ctx->n_vars].name[MAX_IDENT_LEN-1] = '\0';
                    p->ctx->vars[p->ctx->n_vars].value = val;
                    p->ctx->vars[p->ctx->n_vars].is_const = 0;
                    p->ctx->n_vars++;
                }
                    res.kind = STMT_VALUE;
                    res.value = val;
            } else {
                fprintf(stderr, "Assignment requires a numeric value.\n");
            }
            return res;
        }

        if (p->cur.kind == TK_LPAREN) {
            size_t look_i = p->lx.i;
            token_t look_cur = p->cur;
            int depth = 1;
            advance(p);
            while (depth > 0 && p->cur.kind != TK_END) {
                if (p->cur.kind == TK_LPAREN) depth++;
                if (p->cur.kind == TK_RPAREN) depth--;
                advance(p);
            }
            int is_def = (depth == 0 && p->cur.kind == TK_EQ);
            p->lx.i = look_i;
            p->cur = look_cur;
            if (is_def) {
                if (parse_func_def_and_store(p, name)) {
                    res.kind = STMT_DEFINITION;
                    return res;
                } else {
                    fprintf(stderr, "Invalid function definition for %s\n", name);
                    return res;
                }
            }

             /* Special-case: df(src, wrt) -> define derivative function d<src> */
             if (strcmp(name, "df") == 0 && accept(p, TK_LPAREN)) {
                 /* Expect: df(src, wrt) */
                 if (p->cur.kind != TK_IDENT) {
                     fprintf(stderr, "Expected function name in df()\n");
                     return res;
                 }
                 char src[MAX_IDENT_LEN];
                 strncpy(src, p->cur.ident, MAX_IDENT_LEN-1);
                 src[MAX_IDENT_LEN-1] = '\0';
                 advance(p);
                 if (!accept(p, TK_COMMA)) {
                     fprintf(stderr, "Expected ',' in df()\n");
                     return res;
                 }
                 if (p->cur.kind != TK_IDENT) {
                     fprintf(stderr, "Expected variable name in df()\n");
                     return res;
                 }
                 char wrt[MAX_IDENT_LEN];
                 strncpy(wrt, p->cur.ident, MAX_IDENT_LEN-1);
                 wrt[MAX_IDENT_LEN-1] = '\0';
                 advance(p);
                 if (!accept(p, TK_RPAREN)) {
                     fprintf(stderr, "Expected ')' in df()\n");
                     return res;
                 }

                 /* Lookup source function */
                 mp_func *f = lookup_func(p->ctx, src);
                 if (!f) {
                     fprintf(stderr, "Unknown function '%s' in df()\n", src);
                     return res;
                 }

                 /* Compute derivative string (uses diff_module's string-based API) */
                 char *d_body = diff_expr(f->body, wrt);
                 if (!d_body) {
                     fprintf(stderr, "Differentiation failed for %s\n", src);
                     return res;
                 }

                 /* Destination name: d<src> */
                 char dst[MAX_IDENT_LEN * 2];
                 snprintf(dst, sizeof(dst), "d%s", src);

                 /* Define derivative function (define_func will parse into AST & arena) */
                 if (define_func(p->ctx, dst, (const char (*)[MAX_IDENT_LEN])f->params, f->n_params, d_body, strlen(d_body))) {
                     res.kind = STMT_DEFINITION;
                     free(d_body);
                     return res;
                 } else {
                     fprintf(stderr, "Failed to define derivative function %s\n", dst);
                     free(d_body);
                     return res;
                 }
             }
        }

        /* not assignment/definition -> restore */
        p->lx.i = save_i;
        p->cur = save_cur;
    }

    /* parse into malloced AST (not the function's arena) while advancing the real parser */
    Arena *saved_arena = p->cur_arena;
    p->cur_arena = NULL;
    ASTNode *expr = parse_expr_ast(p);
    p->cur_arena = saved_arena;
    double v = eval_ast(expr, p->ctx);
    ast_free(expr);
    if (!isnan(v)) {
        res.kind = STMT_VALUE;
        res.value = v;
    }
    return res;
}

/* ---------------- Program parsing / printing (dynamic stmt buffer) ---------------- */

static double parse_program(parser_t *p) {
    mp_context *ctx = p->ctx;
    if (!ctx) return 0.0;
    double last_value = NAN;
    int has_value = 0;
    while (p->cur.kind != TK_END) {
        size_t stmt_start = p->cur.pos;
        StmtResult r = parse_statement(p);
        size_t stmt_end = p->lx.i;
        if (stmt_start > p->lx.len) stmt_start = p->lx.len;
        if (stmt_end > p->lx.len) stmt_end = p->lx.len;
        size_t len = (stmt_end >= stmt_start) ? (stmt_end - stmt_start) : 0;

        char *stmt = NULL;
        char small_buf[256];
        int used_small = 0;
        if (len == 0) { small_buf[0] = '\0'; used_small = 1; }
        else {
            stmt = malloc(len + 1);
            if (!stmt) {
                size_t slen = (len < sizeof(small_buf)-1) ? len : (sizeof(small_buf)-1);
                if (slen) memcpy(small_buf, p->lx.s + stmt_start, slen);
                small_buf[slen] = '\0';
                used_small = 1;
            } else {
                memcpy(stmt, p->lx.s + stmt_start, len);
                stmt[len] = '\0';
            }
        }

        if (r.kind == STMT_VALUE) {
            if (!ctx->suppress_print) {
                if (used_small) printf("%s => %.17g\n", small_buf, r.value);
                else printf("%s => %.17g\n", stmt, r.value);
            }
            last_value = r.value;
            has_value = 1;
        } else if (r.kind == STMT_DEFINITION) {
            if (!ctx->suppress_print) {
                if (used_small) printf("%s => function defined\n", small_buf);
                else printf("%s => function defined\n", stmt);
        }
            }

        if (stmt) free(stmt);

        accept(p, TK_SEMI);
    }
    return has_value ? last_value : 0.0;
}


/* variable table helpers */

static int lookup_var(mp_context *c, const char *name, double *out)
{
    if (!c)
        return 0;
    for (size_t i = 0; i < c->n_vars; ++i)
    {
        if (strcmp(c->vars[i].name, name) == 0)
        {
            if (out)
                *out = c->vars[i].value;
            return 1;
        }
    }
    return 0;
}
static int set_var(mp_context *c, const char *name, double value)
{
    if (!c)
        return 0;
    for (size_t i = 0; i < c->n_vars; ++i)
    {
        if (strcmp(c->vars[i].name, name) == 0)
        {
            if (c->vars[i].is_const)
            {
                fprintf(stderr, "Error: cannot assign to constant %s\n", name);
                return 0;
            }
            c->vars[i].value = value;
            return 1;
        }
    }
    if (c->n_vars >= c->vars_cap)
    {
        size_t newcap = c->vars_cap ? c->vars_cap * 2 : 64;
        mp_var *tmp = realloc(c->vars, newcap * sizeof(mp_var));
        if (!tmp)
            return 0;
        c->vars = tmp;
        c->vars_cap = newcap;
    }
    strncpy(c->vars[c->n_vars].name, name, MAX_IDENT_LEN - 1);
    c->vars[c->n_vars].name[MAX_IDENT_LEN - 1] = '\0';
    c->vars[c->n_vars].value = value;
    c->vars[c->n_vars].is_const = 0;
    c->n_vars++;
    return 1;
}

static int set_const(mp_context *c, const char *name, double value)
{
    if (!c)
        return 0;
    for (size_t i = 0; i < c->n_vars; ++i)
    {
        if (strcmp(c->vars[i].name, name) == 0)
        {
            c->vars[i].value = value;
            c->vars[i].is_const = 1;
            return 1;
        }
    }
    if (c->n_vars >= c->vars_cap)
    {
        size_t newcap = c->vars_cap ? c->vars_cap * 2 : 64;
        mp_var *tmp = realloc(c->vars, newcap * sizeof(mp_var));
        if (!tmp)
            return 0;
        c->vars = tmp;
        c->vars_cap = newcap;
    }
    strncpy(c->vars[c->n_vars].name, name, MAX_IDENT_LEN - 1);
    c->vars[c->n_vars].name[MAX_IDENT_LEN - 1] = '\0';
    c->vars[c->n_vars].value = value;
    c->vars[c->n_vars].is_const = 1;
    c->n_vars++;
    return 1;
}


/* Reusable parsed expression handle */
typedef struct {
    ASTNode *root;   /* parsed AST root (arena-allocated) */
    Arena *arena;    /* arena that owns the AST (must be freed when done) */
    char *src;       /* optional copied source string (heap) */
} mp_expr;

/* Parse an expression into a new mp_expr whose AST lives in a dedicated arena.
 * Returns NULL on error. Caller must call free_parsed_expr() when done.
 */
mp_expr *parse_expression_into_arena(const char *expr_src)
{
    if (!expr_src) return NULL;
    size_t len = strlen(expr_src);
    /* heuristic block size */
    size_t blk = len > 4096 ? len * 2 : 4096;
    Arena *a = arena_create(blk);
    if (!a) return NULL;

    parser_t p;
    memset(&p, 0, sizeof(p));
    p.lx.s = expr_src;
    p.lx.i = 0;
    p.lx.len = len;
    p.ctx = NULL;          /* no context required for pure parsing */
    p.cur_arena = a;       /* allocate AST nodes & strings in arena */
    advance(&p);           /* prime first token */

    ASTNode *root = parse_expr_ast(&p);
    if (!root) {
        arena_destroy(a);
        return NULL;
    }

    mp_expr *e = malloc(sizeof(mp_expr));
    if (!e) { arena_destroy(a); ast_free(root); return NULL; }
    e->root = root;
    e->arena = a;
    e->src = xstrdup_n(expr_src, len);
    return e;
}

/* Evaluate a parsed expression (no reparsing). ctx may be NULL if expression uses no context. */
double eval_parsed_expr(const mp_expr *e, mp_context *ctx)
{
    if (!e || !e->root) return NAN;
    return eval_ast(e->root, ctx);
}

/* Free a parsed expression (releases its arena and source string). */
void free_parsed_expr(mp_expr *e)
{
    if (!e) return;
    if (e->arena) arena_destroy(e->arena);  /* frees all nodes & strings in arena */
    if (e->src) free(e->src);
    free(e);
}

/* ---------------- Top-level exec API (public) ---------------- */

mp_context* ctx_create(void) {
    mp_context *c = xmalloc(sizeof(mp_context));
    memset(c, 0, sizeof(*c));
    c->trig_mode = 0;
    c->suppress_print = 0;
    c->scratch = arena_create(4096); /* tune initial size */
    return c;
}

void ctx_destroy(mp_context *c) {
    if (!c) return;
    for (size_t i = 0; i < c->n_funcs; ++i) {
        if (c->funcs[i].body) free(c->funcs[i].body);
        if (c->funcs[i].arena) arena_destroy((Arena*)c->funcs[i].arena);
    }
    if (c->scratch) arena_destroy(c->scratch);
    free(c->funcs);
    free(c->vars);
    free(c);
}

double exec_with_ctx(mp_context *ctx, const char *script) {
    parser_t p; memset(&p, 0, sizeof(p));
    p.lx.s = script;
    p.lx.i = 0;
    p.lx.len = strlen(script);
    p.ctx = ctx;
    p.cur_arena = NULL;
    advance(&p);
    return parse_program(&p);
}

double exec(const char *script) {
    mp_context *ctx = ctx_create();
    ctx->suppress_print = 1;
    double r = exec_with_ctx(ctx, script);
    ctx_destroy(ctx);
    return r;
}

/* ---------------- lookup_func (public) ---------------- */

mp_func *lookup_func(mp_context *c, const char *name) {
    if (!c) return NULL;
    for (size_t i = 0; i < c->n_funcs; ++i) {
        if (strcmp(c->funcs[i].name, name) == 0) return &c->funcs[i];
    }
    return NULL;
}

/* ---------------- Demo / small test harness ---------------- */

int mainnot(void)
{
    const char *script =
        "f(a,b)=a*a+b*2;"
        "x=10;"
        "y=3;"
        "f(20,5);"
        "sin(3.141592653589793/2);"
        "DEG(); sin(21); RAD(); sin(21); GRAD(); sin(21); MODE();"
        "sum(3,5,7,9,1,12);"
        "g(x)=x^3+x^2;"
        "diff(g(x),x);"
        "4%3; 10%3; (13%9)+2^(5%3); -10%3;";
    const char* long_script = "sin(1)+cos(1)+tan(1)+log(10)+sqrt(16)+exp(1)+pow(2,8)+abs(-42)+sum(1,2,3,4,5);";  

    
    clock_t start = clock();
    double res = exec(long_script);
     
    clock_t end = clock();
    double cpu_time = ((double)(end - start)) / CLOCKS_PER_SEC;
    printf("Run time: %.6f seconds\n\n", cpu_time);
    printf("Input program:\n%s\n\n", long_script);
    printf("\nLast evaluated value: %.17g\n", res);
    return 0;
}

 

/* put this at end of tinier.c or compile separately linking in parse_expression_into_arena etc. */
int main(void)
{
    const char *expr = "sin(1)+cos(1)+tan(1)+log(10)+sqrt(16)+exp(1)+pow(2,8)+abs(-42)+sum(1,2,3,4,5)";
 

    /* parse once */
    mp_expr *parsed = parse_expression_into_arena(expr);
    if (!parsed) { fprintf(stderr, "Parse failed\n"); return 1; }

    /* create a context if eval needs trig mode / vars etc. */
    mp_context *ctx = ctx_create();
    /* example: set trig mode or any needed globals */
    ctx->trig_mode = 0; /* radians, or 1 for degrees based on your needs */

    /* warm-up (optional) */
    double r = eval_parsed_expr(parsed, ctx);
    (void)r;

    printf("Input program:\n%s\n\n", expr);
    /* timed loop */
    clock_t t0 = clock(); 
      double v = eval_parsed_expr(parsed, ctx);
    clock_t t1 = clock();

       double cpu_time = ((double)(t1 - t0)) / CLOCKS_PER_SEC;
    printf("Run time: %.6f seconds\n\n", cpu_time); 

    /* cleanup */
    free_parsed_expr(parsed);
    ctx_destroy(ctx);
    return 0;
}

/* End of tinier.c */