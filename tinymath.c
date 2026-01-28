/*
 * tinymath.c (v4.1a, cleaned)
 *
 * Full parser with explicit mp_context, ctx_create/ctx_destroy/exec_with_ctx,
 * caller-side suppression of printing during function evaluation, correct
 * user-function numeric evaluation and symbolic handling.
 *
 * Compile:
 *   gcc -O2 -std=c99 -Wall -lm -o tinymath tinymath.c
 *
 * Notes:
 * - Requires parser_api.h and diff_module.h in the same directory.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "parser_api.h"
#include "diff_module.h"

/* suppress_print: when set, parse_program / parse_statement should not print
   evaluation results. We temporarily set it while evaluating a function body. */
static int suppress_print = 0;

/* Local constant for pi */
static const double PI = 3.14159265358979323846;
typedef enum
{
    MODE_RAD,
    MODE_DEG,
    MODE_GRAD
} trig_mode_t;
/* ---------------- Dynamic Variable Table ---------------- */
typedef struct
{
    char name[MAX_IDENT_LEN];
    double value;
    int is_const; // 1 = constant, 0 = normal variable
} mp_var;
/* ---------------- Execution Context ---------------- */

struct mp_context
{
    mp_var *vars;
    size_t n_vars;
    size_t vars_capacity;

    mp_func *funcs;
    size_t n_funcs;
    size_t funcs_capacity;

    int symbolic_mode; /* 1 = symbolic mode enabled */
    trig_mode_t trig_mode;

    int constants_initialized;
};

/* ---------------- Lexer / Parser types ---------------- */

typedef enum
{
    TK_END = 0,
    TK_NUM,
    TK_IDENT,
    TK_PLUS,
    TK_MINUS,
    TK_STAR,
    TK_SLASH,
    TK_PERCENT,
    TK_CARET,
    TK_LPAREN,
    TK_RPAREN,
    TK_COMMA,
    TK_EQ,
    TK_SEMI
} mp_tok_kind;

typedef struct
{
    mp_tok_kind kind;
    double num;
    char ident[MAX_IDENT_LEN];
    size_t pos;
} mp_token;

typedef struct
{
    const char *input;
    size_t i, len;
} mp_lexer;

typedef struct
{
    mp_lexer lx;
    mp_token cur;
    mp_context *ctx; /* explicit context pointer */
} mp_parser;

/* ---------------- Result type ---------------- */

typedef enum { RES_NUM, RES_STR } ResultType;

typedef struct
{
    ResultType type;
    double num;
    char *str;
} mp_result;

/* ------------------ Helpers ------------------ */

static mp_result make_num(double v) { mp_result r = {RES_NUM, v, NULL}; return r; }
static mp_result make_str(const char *s) { mp_result r = {RES_STR, NAN, strdup(s)}; return r; }

static char *double_to_string(double v)
{
    char buf[MAX_IDENT_LEN];
    snprintf(buf, sizeof(buf), "%.17g", v);
    return strdup(buf);
}

static char *concat3(const char *a, const char *b, const char *c)
{
    size_t len = strlen(a) + strlen(b) + strlen(c) + 1;
    char *out = malloc(len);
    if (!out) return NULL;
    snprintf(out, len, "%s%s%s", a, b, c);
    return out;
}


/* ---------------- Forward declarations ---------------- */

static mp_result parse_term(mp_parser *p);
static mp_result parse_factor(mp_parser *p);
static mp_result parse_primary(mp_parser *p);
static mp_result parse_expr(mp_parser *p);
static double parse_program(mp_parser *p);

/* Binary ops (symbolic-aware) */
#define DEFINE_BINOP_EXT(name, opstr, numeric_expr)                               \
    static mp_result name(mp_result a, mp_result b)                               \
    {                                                                             \
        if (a.type == RES_NUM && b.type == RES_NUM)                               \
            return make_num(numeric_expr);                                        \
        char *sa = (a.type == RES_STR) ? strdup(a.str) : double_to_string(a.num); \
        char *sb = (b.type == RES_STR) ? strdup(b.str) : double_to_string(b.num); \
        if (a.type == RES_STR)                                                    \
        {                                                                         \
            char *t = sa;                                                         \
            sa = malloc(strlen(t) + 3);                                           \
            sprintf(sa, "(%s)", t);                                               \
            free(t);                                                              \
        }                                                                         \
        if (b.type == RES_STR)                                                    \
        {                                                                         \
            char *t = sb;                                                         \
            sb = malloc(strlen(t) + 3);                                           \
            sprintf(sb, "(%s)", t);                                               \
            free(t);                                                              \
        }                                                                         \
        char *combined = concat3(sa, opstr, sb);                                  \
        free(sa); free(sb);                                                       \
        return make_str(combined);                                                \
    }

DEFINE_BINOP_EXT(add, " + ", a.num + b.num)
DEFINE_BINOP_EXT(sub, " - ", a.num - b.num)
DEFINE_BINOP_EXT(mul, " * ", a.num * b.num)
DEFINE_BINOP_EXT(divide, " / ", a.num / b.num)
DEFINE_BINOP_EXT(mod_result, " % ", fmod(a.num, b.num))
DEFINE_BINOP_EXT(pow_result, "^", pow(a.num, b.num))

/* ---------------- Lexer ---------------- */

static void skip_ws(mp_lexer *lx)
{
    for (;;)
    {
        while (lx->i < lx->len && isspace((unsigned char)lx->input[lx->i])) lx->i++;
        if (lx->i < lx->len && lx->input[lx->i] == '#')
        {
            while (lx->i < lx->len && lx->input[lx->i] != '\n') lx->i++;
            continue;
        }
        if (lx->i + 1 < lx->len && lx->input[lx->i] == '/' && lx->input[lx->i+1] == '/')
        {
            lx->i += 2;
            while (lx->i < lx->len && lx->input[lx->i] != '\n') lx->i++;
            continue;
        }
        if (lx->i + 1 < lx->len && lx->input[lx->i] == '/' && lx->input[lx->i+1] == '*')
        {
            lx->i += 2;
            while (lx->i + 1 < lx->len && !(lx->input[lx->i] == '*' && lx->input[lx->i+1] == '/')) lx->i++;
            if (lx->i + 1 < lx->len) lx->i += 2;
            continue;
        }
        break;
    }
}

static mp_token next_token(mp_lexer *lx)
{
    skip_ws(lx);
    mp_token t = {0};
    t.pos = lx->i;
    if (lx->i >= lx->len) { t.kind = TK_END; return t; }
    char c = lx->input[lx->i];

    if (isdigit((unsigned char)c) || (c == '.' && lx->i + 1 < lx->len && isdigit((unsigned char)lx->input[lx->i+1])))
    {
        char buf[128];
        size_t j = 0;
        int seen_dot = 0;
        int seen_exp = 0;

        while (lx->i < lx->len)
        {
            char cc = lx->input[lx->i];
            if (isdigit((unsigned char)cc))
            {
                if (j < sizeof(buf)-1) buf[j++] = cc;
                lx->i++;
                continue;
            }
            if ((cc == '.' ) && !seen_dot && !seen_exp)
            {
                seen_dot = 1;
                if (j < sizeof(buf)-1) buf[j++] = cc;
                lx->i++;
                continue;
            }
            if ((cc == 'e' || cc == 'E') && !seen_exp)
            {
                seen_exp = 1;
                if (j < sizeof(buf)-1) buf[j++] = cc;
                lx->i++;
                if (lx->i < lx->len && (lx->input[lx->i] == '+' || lx->input[lx->i] == '-'))
                {
                    if (j < sizeof(buf)-1) buf[j++] = lx->input[lx->i];
                    lx->i++;
                }
                continue;
            }
            break;
        }

        buf[j] = '\0';
        t.kind = TK_NUM;
        t.num = strtod(buf, NULL);
        return t;
    }

    if (isalpha((unsigned char)c) || c == '_')
    {
        char buf[MAX_IDENT_LEN];
        size_t j = 0;
        while (lx->i < lx->len && (isalnum((unsigned char)lx->input[lx->i]) || lx->input[lx->i] == '_'))
        {
            if (j < sizeof(buf)-1) buf[j++] = lx->input[lx->i];
            lx->i++;
        }
        buf[j] = '\0';
        t.kind = TK_IDENT;
        snprintf(t.ident, sizeof(t.ident), "%s", buf);
        return t;
    }

    lx->i++;
    switch (c)
    {
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
        default:  t.kind = TK_END; break;
    }
    return t;
}

static void advance(mp_parser *p) { p->cur = next_token(&p->lx); }
static int accept(mp_parser *p, mp_tok_kind k) { if (p->cur.kind == k) { advance(p); return 1; } return 0; }

/* ---------------- Context-backed variable & function table ---------------- */

static int add_var(mp_context *ctx, const char *name, double value, int is_const)
{
    if (!ctx) return 0;

    for (size_t i = 0; i < ctx->n_vars; i++)
    {
        if (strcmp(ctx->vars[i].name, name) == 0)
        {
            fprintf(stderr, "Error: redefining %s\n", name);
            return 0;
        }
    }
    if (ctx->n_vars >= ctx->vars_capacity)
    {
        size_t new_cap = ctx->vars_capacity ? ctx->vars_capacity * 2 : 16;
        mp_var *temp = realloc(ctx->vars, new_cap * sizeof(mp_var));
        if (!temp)
        {
            fprintf(stderr, "Memory allocation failed for variables\n");
            return 0;
        }
        ctx->vars = temp;
        ctx->vars_capacity = new_cap;
    }
    snprintf(ctx->vars[ctx->n_vars].name, sizeof(ctx->vars[ctx->n_vars].name), "%s", name);
    ctx->vars[ctx->n_vars].value = value;
    ctx->vars[ctx->n_vars].is_const = is_const;
    ctx->n_vars++;
    return 1;
}

int set_const(mp_context *ctx, const char *name, double value)
{
    return add_var(ctx, name, value, 1);
}

int set_var(mp_context *ctx, const char *name, double value)
{
    if (!ctx) return 0;
    for (size_t i = 0; i < ctx->n_vars; i++)
    {
        if (strcmp(ctx->vars[i].name, name) == 0)
        {
            if (ctx->vars[i].is_const)
            {
                fprintf(stderr, "Error: cannot assign to constant %s\n", name);
                return 0;
            }
            ctx->vars[i].value = value;
            return 1;
        }
    }
    return add_var(ctx, name, value, 0);
}

static int lookup_var(mp_context *ctx, const char *name, double *out)
{
    if (!ctx) return 0;
    for (size_t i = 0; i < ctx->n_vars; i++)
    {
        if (strcmp(ctx->vars[i].name, name) == 0)
        {
            *out = ctx->vars[i].value;
            return 1;
        }
    }
    return 0;
}

/* Remove a variable by name from the context (shifts array down). */
static void remove_var(mp_context *ctx, const char *name)
{
    if (!ctx) return;
    for (size_t i = 0; i < ctx->n_vars; ++i)
    {
        if (strcmp(ctx->vars[i].name, name) == 0)
        {
            for (size_t j = i + 1; j < ctx->n_vars; ++j)
                ctx->vars[j - 1] = ctx->vars[j];
            ctx->n_vars--;
            return;
        }
    }
}

/* ---------------- Function table (context-backed) ---------------- */

int define_func(mp_context *ctx, const char *name, const char params[][MAX_IDENT_LEN], int n_params,
                const char *body_start, size_t body_len)
{
    if (!ctx) return 0;

    if (n_params > MAX_FUNC_PARAMS)
    {
        fprintf(stderr, "Too many parameters (max %d)\n", MAX_FUNC_PARAMS);
        return 0;
    }

    char *body_copy = malloc(body_len + 1);
    if (!body_copy)
    {
        fprintf(stderr, "Memory allocation failed for function body\n");
        return 0;
    }
    memcpy(body_copy, body_start, body_len);
    body_copy[body_len] = '\0';

    if (ctx->n_funcs >= ctx->funcs_capacity)
    {
        size_t new_cap = ctx->funcs_capacity ? ctx->funcs_capacity * 2 : 16;
        mp_func *temp = realloc(ctx->funcs, new_cap * sizeof(mp_func));
        if (!temp)
        {
            free(body_copy);
            fprintf(stderr, "Memory allocation failed for function table\n");
            return 0;
        }
        ctx->funcs = temp;
        ctx->funcs_capacity = new_cap;
    }

    mp_func *f = &ctx->funcs[ctx->n_funcs++];
    snprintf(f->name, sizeof(f->name), "%s", name);
    f->n_params = n_params;
    for (int i = 0; i < n_params; i++)
    {
        snprintf(f->params[i], sizeof(f->params[i]), "%s", params[i]);
    }
    f->body = body_copy;

    return 1;
}

mp_func *lookup_func(mp_context *ctx, const char *name)
{
    if (!ctx) return NULL;
    for (size_t i = 0; i < ctx->n_funcs; i++)
    {
        if (strcmp(ctx->funcs[i].name, name) == 0)
            return &ctx->funcs[i];
    }
    return NULL;
}

/* ---------------- Sorting helper ---------------- */

static int double_cmp(const void *aa, const void *bb)
{
    double a = *(const double *)aa;
    double b = *(const double *)bb;

    int a_nan = isnan(a);
    int b_nan = isnan(b);

    if (a_nan || b_nan)
    {
        if (a_nan && b_nan) return 0;
        if (a_nan) return 1;
        return -1;
    }

    return (a > b) - (a < b);
}

/* ---------------- Statistical functions (variable arity) ---------------- */

static double call_stat(const char *name, double *args, int n_args)
{
    if (n_args <= 0) return NAN;

    double min_v = args[0], max_v = args[0];
    double sum = 0, prod = 1, sum_sq = 0;

    for (int i = 0; i < n_args; i++)
    {
        double v = args[i];
        if (v < min_v) min_v = v;
        if (v > max_v) max_v = v;
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
        if (std_pop == 0) return 0.0;
        return (args[0] - mean) / std_pop;
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
                if (sorted[i] == sorted[i - 1]) cur_c++;
                else
                {
                    if (cur_c > max_c) { max_c = cur_c; mode_val = sorted[i - 1]; }
                    cur_c = 1;
                }
            }
            if (cur_c > max_c) mode_val = sorted[n_args - 1];
            free(sorted);
        }
    }

    if (strcmp(name, "min") == 0) return min_v;
    if (strcmp(name, "max") == 0) return max_v;
    if (strcmp(name, "sum") == 0) return sum;
    if (strcmp(name, "mean") == 0 || strcmp(name, "avg") == 0) return mean;
    if (strcmp(name, "rng") == 0) return max_v - min_v;
    if (strcmp(name, "mrng") == 0) return (min_v + max_v) / 2.0;
    if (strcmp(name, "std_err") == 0 || strcmp(name, "sem") == 0) return std_sam / sqrt((double)n_args);
    if (strcmp(name, "var") == 0 || strcmp(name, "pvar") == 0) return var_pop;
    if (strcmp(name, "std") == 0 || strcmp(name, "pstd") == 0) return sqrt(var_pop);
    if (strcmp(name, "svar") == 0) return var_sam;
    if (strcmp(name, "sstd") == 0) return std_sam;
    if (strcmp(name, "median") == 0 || strcmp(name, "med") == 0) return median;
    if (strcmp(name, "mode") == 0) return mode_val;
    if (strcmp(name, "rms") == 0) return sqrt(sum_sq / n_args);

    return NAN;
}

/* ---------------- Built-in functions (accept ctx for trig mode side-effects) ---------------- */

static double call_builtin(mp_context *ctx, const char *name, double *args, int n)
{
    if (n == 0)
    {
        if (strcasecmp(name, "DEG") == 0) { if (ctx) ctx->trig_mode = MODE_DEG; if(!suppress_print) printf("Mode: DEG\n"); return NAN; }
        if (strcasecmp(name, "RAD") == 0) { if (ctx) ctx->trig_mode = MODE_RAD; if(!suppress_print) printf("Mode: RAD\n"); return NAN; }
        if (strcasecmp(name, "GRAD") == 0) { if (ctx) ctx->trig_mode = MODE_GRAD; if(!suppress_print) printf("Mode: GRAD\n"); return NAN; }
        if (strcasecmp(name, "MODE") == 0) {
            if (!ctx) { if(!suppress_print) printf("Mode: ?\n"); return NAN; }
            const char *m = (ctx->trig_mode==MODE_DEG) ? "DEG" : (ctx->trig_mode==MODE_GRAD) ? "GRAD" : "RAD";
            if(!suppress_print) printf("Mode: %s\n", m);
            return NAN;
        }
    }

    if (strcmp(name, "sin") == 0 && n == 1)
        return sin((ctx ? (ctx->trig_mode==MODE_DEG ? args[0]*PI/180.0 : ctx->trig_mode==MODE_GRAD ? args[0]*PI/200.0 : args[0]) : args[0]));
    if (strcmp(name, "cos") == 0 && n == 1)
        return cos((ctx ? (ctx->trig_mode==MODE_DEG ? args[0]*PI/180.0 : ctx->trig_mode==MODE_GRAD ? args[0]*PI/200.0 : args[0]) : args[0]));
    if (strcmp(name, "tan") == 0 && n == 1)
        return tan((ctx ? (ctx->trig_mode==MODE_DEG ? args[0]*PI/180.0 : ctx->trig_mode==MODE_GRAD ? args[0]*PI/200.0 : args[0]) : args[0]));
    if (strcmp(name, "sqrt") == 0 && n == 1) return sqrt(args[0]);
    if ( (strcmp(name, "log") == 0 || strcmp(name, "ln") == 0) && n == 1) return log(args[0]);
    if (strcmp(name, "exp") == 0 && n == 1) return exp(args[0]);
    if (strcmp(name, "abs") == 0 && n == 1) return fabs(args[0]);
    if (strcmp(name, "pow") == 0 && n == 2) return pow(args[0], args[1]);
    return NAN;
}

/* ---------------- call_user_func ---------------- */

static mp_result call_user_func(mp_context *ctx, mp_func *f, double *args, int n)
{
    if (!ctx || !f) return make_num(NAN);

    if (n != f->n_params)
    {
        fprintf(stderr, "Wrong number of arguments for %s (got %d, expected %d)\n",
                f->name, n, f->n_params);
        return make_num(NAN);
    }

    double *oldvals = malloc(sizeof(double) * f->n_params);
    int *had_old = malloc(sizeof(int) * f->n_params);
    int *new_param = malloc(sizeof(int) * f->n_params);
    if (!oldvals || !had_old || !new_param)
    {
        fprintf(stderr, "Memory allocation failed in call_user_func\n");
        free(oldvals); free(had_old); free(new_param);
        return make_num(NAN);
    }

    for (int i = 0; i < f->n_params; ++i)
    {
        had_old[i] = lookup_var(ctx, f->params[i], &oldvals[i]);
        new_param[i] = 0;
    }

    for (int i = 0; i < n; ++i)
    {
        if (!had_old[i])
            new_param[i] = 1;
        set_var(ctx, f->params[i], args[i]);
    }

    int old_suppress = suppress_print;
    suppress_print = 1;

    mp_parser sub = {{f->body, 0, strlen(f->body)}, {0}, ctx};
    advance(&sub);
    double last_val = parse_program(&sub);

    suppress_print = old_suppress;

    mp_result result = make_num(last_val);

    for (int i = 0; i < n; ++i)
    {
        if (had_old[i])
        {
            set_var(ctx, f->params[i], oldvals[i]);
        }
        else if (new_param[i])
        {
            remove_var(ctx, f->params[i]);
        }
    }

    free(oldvals);
    free(had_old);
    free(new_param);
    return result;
}

/* ---------------- Parsing: primary / factor / term / expr ---------------- */

static mp_result parse_primary(mp_parser *p)
{
    mp_context *ctx = p->ctx;

    if (p->cur.kind == TK_NUM)
    {
        double v = p->cur.num;
        advance(p);
        return make_num(v);
    }

    if (p->cur.kind == TK_IDENT)
    {
        char name[MAX_IDENT_LEN];
        snprintf(name, sizeof(name), "%s", p->cur.ident);
        advance(p);

        if (strcmp(name, "diff") == 0 && accept(p, TK_LPAREN))
        {
            if (ctx) ctx->symbolic_mode = 1;
            mp_result inner = parse_expr(p);
            if (ctx) ctx->symbolic_mode = 0;

            if (!accept(p, TK_COMMA))
            {
                fprintf(stderr, "Expected ',' in diff()\n");
                if (inner.type == RES_STR) free(inner.str);
                return make_num(NAN);
            }

            if (p->cur.kind != TK_IDENT)
            {
                fprintf(stderr, "Expected variable name after ',' in diff()\n");
                if (inner.type == RES_STR) free(inner.str);
                return make_num(NAN);
            }
            char var[MAX_IDENT_LEN];
            snprintf(var, sizeof(var), "%s", p->cur.ident);
            advance(p);

            if (accept(p, TK_COMMA))
            {
                mp_result point = parse_expr(p);
                if (!accept(p, TK_RPAREN))
                {
                    fprintf(stderr, "Expected ')' in diff()\n");
                    if (inner.type == RES_STR) free(inner.str);
                    if (point.type == RES_STR) free(point.str);
                    return make_num(NAN);
                }
                if (point.type != RES_NUM)
                {
                    fprintf(stderr, "Evaluation point must be numeric\n");
                    if (inner.type == RES_STR) free(inner.str);
                    if (point.type == RES_STR) free(point.str);
                    return make_num(NAN);
                }

                char *expr_str = (inner.type == RES_NUM) ? double_to_string(inner.num) : strdup(inner.str);
                char *deriv_str = NULL;
                if (ctx)
                    deriv_str = diff_expr_ctx(ctx, expr_str, var);
                else
                    deriv_str = diff_expr(expr_str, var);

                free(expr_str);
                if (inner.type == RES_STR) free(inner.str);

                double old_val = 0.0;
                int had_var = 0;
                if (ctx)
                    had_var = lookup_var(ctx, var, &old_val);

                if (ctx)
                    set_var(ctx, var, point.num);

                mp_parser sub = {{deriv_str, 0, strlen(deriv_str)}, {0}, ctx};
                advance(&sub);
                mp_result result = parse_expr(&sub);

                if (ctx)
                {
                    if (had_var)
                        set_var(ctx, var, old_val);
                    else
                        remove_var(ctx, var);
                }

                free(deriv_str);
                return result;
            }
            else
            {
                if (!accept(p, TK_RPAREN))
                {
                    fprintf(stderr, "Expected ')' in diff()\n");
                    if (inner.type == RES_STR) free(inner.str);
                    return make_num(NAN);
                }

                char *expr_str = (inner.type == RES_NUM) ? double_to_string(inner.num) : strdup(inner.str);
                char *deriv_str = NULL;
                if (ctx)
                    deriv_str = diff_expr_ctx(ctx, expr_str, var);
                else
                    deriv_str = diff_expr(expr_str, var);

                free(expr_str);
                if (inner.type == RES_STR) free(inner.str);

                return make_str(deriv_str);
            }
        }

        if (accept(p, TK_LPAREN))
        {
            mp_result *args = NULL;
            int n_args = 0;
            if (!accept(p, TK_RPAREN))
            {
                do
                {
                    mp_result r = parse_expr(p);
                    mp_result *tmp = realloc(args, (n_args + 1) * sizeof(mp_result));
                    if (!tmp)
                    {
                        fprintf(stderr, "Memory allocation failed while parsing arguments\n");
                        for (int j = 0; j < n_args; ++j) if (args[j].type == RES_STR) free(args[j].str);
                        free(args);
                        return make_num(NAN);
                    }
                    args = tmp;
                    args[n_args++] = r;
                } while (accept(p, TK_COMMA));
                if (!accept(p, TK_RPAREN))
                {
                    fprintf(stderr, "Missing ')' in function call\n");
                    for (int j = 0; j < n_args; ++j) if (args[j].type == RES_STR) free(args[j].str);
                    free(args);
                    return make_num(NAN);
                }
            }

            int all_numeric = 1;
            for (int i = 0; i < n_args; i++) if (args[i].type != RES_NUM) all_numeric = 0;

            char **arg_strs = NULL;
            double *num_args = NULL;
            if (n_args > 0)
            {
                arg_strs = malloc(sizeof(char *) * n_args);
                num_args = malloc(sizeof(double) * n_args);
                if (!arg_strs || !num_args)
                {
                    fprintf(stderr, "Memory allocation failed for function call args\n");
                    for (int j = 0; j < n_args; ++j) if (args[j].type == RES_STR) free(args[j].str);
                    free(args);
                    free(arg_strs);
                    free(num_args);
                    return make_num(NAN);
                }
            }

            for (int i = 0; i < n_args; i++)
            {
                if (args[i].type == RES_NUM)
                {
                    arg_strs[i] = double_to_string(args[i].num);
                    num_args[i] = args[i].num;
                }
                else
                {
                    arg_strs[i] = strdup(args[i].str);
                }
            }

            mp_func *uf = lookup_func(ctx, name);
            if (uf)
            {
                if (all_numeric)
                {
                    double *call_args = malloc(sizeof(double) * n_args);
                    if (!call_args)
                    {
                        fprintf(stderr, "Memory allocation failed for call_args\n");
                        for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                        free(arg_strs); free(num_args);
                        for (int i = 0; i < n_args; ++i) if (args[i].type == RES_STR) free(args[i].str);
                        free(args);
                        return make_num(NAN);
                    }
                    for (int i = 0; i < n_args; i++) call_args[i] = num_args[i];

                    mp_result res = call_user_func(ctx, uf, call_args, n_args);
                    free(call_args);

                    for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                    free(arg_strs); free(num_args);
                    for (int i = 0; i < n_args; ++i) if (args[i].type == RES_STR) free(args[i].str);
                    free(args);

                    return res;
                }
                else
                {
                    size_t len = strlen(name) + 3;
                    for (int i = 0; i < n_args; i++) len += strlen(arg_strs[i]) + (i > 0 ? 2 : 0);
                    char *combined = malloc(len);
                    if (!combined)
                    {
                        fprintf(stderr, "Memory allocation failed for symbolic user function\n");
                        for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                        free(arg_strs); free(num_args);
                        for (int i = 0; i < n_args; ++i) if (args[i].type == RES_STR) free(args[i].str);
                        free(args);
                        return make_num(NAN);
                    }
                    snprintf(combined, len, "%s(", name);
                    for (int i = 0; i < n_args; i++)
                    {
                        if (i > 0) strcat(combined, ", ");
                        strcat(combined, arg_strs[i]);
                    }
                    strcat(combined, ")");

                    for (int i = 0; i < n_args; i++)
                    {
                        free(arg_strs[i]);
                        if (args[i].type == RES_STR) free(args[i].str);
                    }
                    free(arg_strs);
                    free(num_args);
                    free(args);

                    return make_str(combined);
                }
            }

            if (all_numeric)
            {
                double *call_args = malloc(sizeof(double) * n_args);
                if (!call_args)
                {
                    fprintf(stderr, "Memory allocation failed for call_args\n");
                    for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                    free(arg_strs); free(num_args);
                    for (int i = 0; i < n_args; ++i) if (args[i].type == RES_STR) free(args[i].str);
                    free(args);
                    return make_num(NAN);
                }
                for (int i = 0; i < n_args; i++) call_args[i] = num_args[i];

                double val = call_builtin(ctx, name, call_args, n_args);
                if (isnan(val)) val = call_stat(name, call_args, n_args);

                free(call_args);
                for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                free(arg_strs); free(num_args);
                for (int i = 0; i < n_args; ++i) if (args[i].type == RES_STR) free(args[i].str);
                free(args);

                if (!isnan(val)) return make_num(val);
            }

            {
                size_t len = strlen(name) + 3;
                for (int i = 0; i < n_args; i++) len += strlen(arg_strs[i]) + (i > 0 ? 2 : 0);
                char *combined = malloc(len);
                if (!combined)
                {
                    fprintf(stderr, "Memory allocation failed for symbolic fallback\n");
                    for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                    free(arg_strs); free(num_args);
                    for (int i = 0; i < n_args; ++i) if (args[i].type == RES_STR) free(args[i].str);
                    free(args);
                    return make_num(NAN);
                }
                snprintf(combined, len, "%s(", name);
                for (int i = 0; i < n_args; i++)
                {
                    if (i > 0) strcat(combined, ", ");
                    strcat(combined, arg_strs[i]);
                }
                strcat(combined, ")");
                for (int i = 0; i < n_args; i++)
                {
                    free(arg_strs[i]);
                    if (args[i].type == RES_STR) free(args[i].str);
                }
                free(arg_strs); free(num_args); free(args);
                return make_str(combined);
            }
        }

        if (ctx && ctx->symbolic_mode)
            return make_str(name);

        double v;
        if (lookup_var(ctx, name, &v))
            return make_num(v);

        fprintf(stderr, "Unknown variable: %s\n", name);
        return make_num(NAN);
    }

    if (accept(p, TK_LPAREN))
    {
        mp_result r = parse_expr(p);
        if (!accept(p, TK_RPAREN)) fprintf(stderr, "Missing ')'\n");
        return r;
    }

    if (accept(p, TK_MINUS))
    {
        mp_result r = parse_primary(p);
        if (r.type == RES_NUM) r.num = -r.num;
        else
        {
            char *s = malloc(strlen(r.str) + 4);
            sprintf(s, "-(%s)", r.str);
            free(r.str);
            r.str = s;
        }
        return r;
    }

    if (accept(p, TK_PLUS))
        return parse_primary(p);

    fprintf(stderr, "Unexpected token in primary (pos %zu)\n", p->lx.i);
    return make_num(NAN);
}

static mp_result parse_factor(mp_parser *p)
{
    mp_result v = parse_primary(p);
    if (accept(p, TK_CARET))
    {
        mp_result rhs = parse_factor(p);
        v = pow_result(v, rhs);
    }
    return v;
}

static mp_result parse_term(mp_parser *p)
{
    mp_result v = parse_factor(p);
    while (p->cur.kind == TK_STAR || p->cur.kind == TK_SLASH || p->cur.kind == TK_PERCENT)
    {
        mp_tok_kind op = p->cur.kind;
        advance(p);
        mp_result rhs = parse_factor(p);
        if (op == TK_STAR) v = mul(v, rhs);
        else if (op == TK_SLASH) v = divide(v, rhs);
        else v = mod_result(v, rhs);
    }
    return v;
}

static mp_result parse_expr(mp_parser *p)
{
    mp_result v = parse_term(p);
    while (p->cur.kind == TK_PLUS || p->cur.kind == TK_MINUS)
    {
        mp_tok_kind op = p->cur.kind;
        advance(p);
        mp_result rhs = parse_term(p);
        v = (op == TK_PLUS) ? add(v, rhs) : sub(v, rhs);
    }
    return v;
}

/* ---------------- Function definition parsing ---------------- */

static int parse_func_def(mp_parser *p, const char *fname)
{
    mp_context *ctx = p->ctx;
    if (!accept(p, TK_LPAREN))
    {
        fprintf(stderr, "Error: Expected '(' after function name '%s'\n", fname);
        return 0;
    }

    char params[MAX_FUNC_PARAMS][MAX_IDENT_LEN];
    int n_params = 0;

    while (p->cur.kind == TK_IDENT)
    {
        if (n_params >= MAX_FUNC_PARAMS)
        {
            fprintf(stderr, "Error: Function '%s' exceeds limit of %d parameters\n", fname, MAX_FUNC_PARAMS);
            return 0;
        }

        snprintf(params[n_params++], MAX_IDENT_LEN, "%s", p->cur.ident);
        advance(p);

        if (p->cur.kind == TK_COMMA)
        {
            advance(p);
            if (p->cur.kind != TK_IDENT)
            {
                fprintf(stderr, "Error: Trailing comma in parameter list of '%s'\n", fname);
                return 0;
            }
        }
        else
        {
            break;
        }
    }

    if (!accept(p, TK_RPAREN))
    {
        fprintf(stderr, "Error: Expected ')' after parameters in '%s'\n", fname);
        return 0;
    }
    if (!accept(p, TK_EQ))
    {
        fprintf(stderr, "Error: Expected '=' to define function '%s'\n", fname);
        return 0;
    }

    size_t body_start_idx = p->cur.pos;
    while (p->cur.kind != TK_SEMI && p->cur.kind != TK_END)
        advance(p);
    size_t body_end_idx = p->cur.pos;
    size_t body_len = 0;
    if (body_end_idx > body_start_idx)
        body_len = body_end_idx - body_start_idx;

    if (p->cur.kind == TK_SEMI)
        advance(p);

    return define_func(ctx, fname, params, n_params, p->lx.input + body_start_idx, body_len);
}

/* ---------------- Statement ---------------- */

typedef enum { STMT_VALUE, STMT_DEFINITION, STMT_ERROR } StmtResultKind;
typedef struct { StmtResultKind kind; double value; } StmtResult;

/* parse_statement defined earlier used by parse_program */

static StmtResult parse_statement(mp_parser *p); /* forward declaration used above */

static StmtResult parse_statement(mp_parser *p)
{
    mp_context *ctx = p->ctx;
    StmtResult res = {STMT_ERROR, NAN};

    if (p->cur.kind == TK_IDENT)
    {
        char name[MAX_IDENT_LEN];
        snprintf(name, sizeof(name), "%s", p->cur.ident);

        size_t save_pre_i = p->lx.i;
        mp_token save_pre_cur = p->cur;

        advance(p);

        if (p->cur.kind == TK_EQ)
        {
            advance(p);
            mp_result val = parse_expr(p);
            if (val.type == RES_NUM && !isnan(val.num))
            {
                if (set_var(ctx, name, val.num))
                {
                    res.kind = STMT_VALUE;
                    res.value = val.num;
                }
            }
            else
            {
                fprintf(stderr, "Assignment requires a numeric value.\n");
            }
            if (val.type == RES_STR)
                free(val.str);
            return res;
        }

        if (p->cur.kind == TK_LPAREN)
        {
            size_t save_after_i = p->lx.i;
            mp_token save_after_cur = p->cur;

            int paren_depth = 1;
            advance(p);
            while (paren_depth > 0 && p->cur.kind != TK_END)
            {
                if (p->cur.kind == TK_LPAREN)
                    paren_depth++;
                if (p->cur.kind == TK_RPAREN)
                    paren_depth--;
                advance(p);
            }

            int is_definition = (paren_depth == 0 && p->cur.kind == TK_EQ);

            p->lx.i = save_after_i;
            p->cur = save_after_cur;

            if (is_definition)
            {
                if (parse_func_def(p, name))
                {
                    res.kind = STMT_DEFINITION;
                    return res;
                }
                fprintf(stderr, "Invalid function definition for %s\n", name);
                return res;
            }
        }

        if (strcmp(name, "df") == 0 && accept(p, TK_LPAREN))
        {
            if (p->cur.kind == TK_IDENT)
            {
                char src[MAX_IDENT_LEN];
                snprintf(src, sizeof(src), "%s", p->cur.ident);
                advance(p);
                if (!accept(p, TK_COMMA))
                {
                    fprintf(stderr, "Expected ',' in df()\n");
                    return res;
                }
                if (p->cur.kind == TK_IDENT)
                {
                    char wrt[MAX_IDENT_LEN];
                    snprintf(wrt, sizeof(wrt), "%s", p->cur.ident);
                    advance(p);
                    if (!accept(p, TK_RPAREN))
                    {
                        fprintf(stderr, "Expected ')' in df()\n");
                        return res;
                    }
                    char dst[128];
                    snprintf(dst, sizeof(dst), "d%s", src);
                    if (diff_func(ctx, src, wrt, dst))
                    {
                        res.kind = STMT_DEFINITION;
                        return res;
                    }
                }
            }
        }

        p->lx.i = save_pre_i;
        p->cur = save_pre_cur;
    }

    mp_result v = parse_expr(p);

    if (v.type == RES_NUM && !isnan(v.num))
    {
        res.kind = STMT_VALUE;
        res.value = v.num;
    }
    else if (v.type == RES_STR)
    {
        if (!suppress_print) printf("%s\n", v.str);
        free(v.str);
    }

    if ((v.type == RES_NUM && isnan(v.num)) || (v.type == RES_STR && v.str == NULL))
    {
        while (p->cur.kind != TK_SEMI && p->cur.kind != TK_END)
            advance(p);
        if (p->cur.kind == TK_SEMI)
            advance(p);
    }

    return res;
}

/* ---------------- Constants & program parsing ---------------- */

void init_constants(mp_context *ctx)
{
    if (!ctx) return;
    set_const(ctx, "pi", PI);
    set_const(ctx, "e", 2.71828182845904523536);
    set_const(ctx, "c", 299792458.0);
    set_const(ctx, "k", 1.380649e-23);
    set_const(ctx, "h", 6.62607015e-34);
    set_const(ctx, "G", 6.67430e-11);
    set_const(ctx, "Na", 6.02214076e23);
}

static double parse_program(mp_parser *p)
{
    mp_context *ctx = p->ctx;
    if (!ctx) return 0.0;

    if (!ctx->constants_initialized)
    {
        init_constants(ctx);
        ctx->constants_initialized = 1;
    }

    double last_value = NAN;
    int has_value = 0;

    while (p->cur.kind != TK_END)
    {
        size_t stmt_start = p->cur.pos;
        StmtResult r = parse_statement(p);
        size_t stmt_end = p->lx.i;

        size_t len = stmt_end - stmt_start;
        char stmt[256];
        if (len >= sizeof(stmt)) len = sizeof(stmt) - 1;
        memcpy(stmt, p->lx.input + stmt_start, len);
        stmt[len] = '\0';

        if (r.kind == STMT_VALUE)
        {
            if (!suppress_print) printf("%s => %.17g\n", stmt, r.value);
            last_value = r.value;
            has_value = 1;
        }
        else if (r.kind == STMT_DEFINITION)
        {
            if (!suppress_print) printf("%s => function defined\n", stmt);
        }

        accept(p, TK_SEMI);
    }

    return has_value ? last_value : 0.0;
}

/* ---------------- Context management & exec API (v4.1) ---------------- */

mp_context* ctx_create(void)
{
    mp_context *ctx = (mp_context*)calloc(1, sizeof(mp_context));
    if (!ctx) return NULL;
    ctx->vars = NULL;
    ctx->n_vars = 0;
    ctx->vars_capacity = 0;
    ctx->funcs = NULL;
    ctx->n_funcs = 0;
    ctx->funcs_capacity = 0;
    ctx->symbolic_mode = 0;
    ctx->trig_mode = MODE_RAD;
    ctx->constants_initialized = 0;
    return ctx;
}

void ctx_destroy(mp_context *ctx)
{
    if (!ctx) return;
    for (size_t i = 0; i < ctx->n_funcs; ++i)
        free(ctx->funcs[i].body);
    free(ctx->funcs);
    ctx->funcs = NULL;
    ctx->n_funcs = 0;
    ctx->funcs_capacity = 0;

    free(ctx->vars);
    ctx->vars = NULL;
    ctx->n_vars = 0;
    ctx->vars_capacity = 0;

    free(ctx);
}

double exec_with_ctx(mp_context *ctx, const char *script)
{
    if (!ctx || !script) return 0.0;
    mp_parser p = {{script, 0, strlen(script)}, {0}, ctx};
    advance(&p);
    return parse_program(&p);
}

double exec(const char *script)
{
    mp_context *ctx = ctx_create();
    if (!ctx) return 0.0;
    double res = exec_with_ctx(ctx, script);
    ctx_destroy(ctx);
    return res;
}

/* ---------------- Demo main ---------------- */

int main(void)
{
    const char *script =
        "f(a,b)=a*a+b*2;"
        "x=10;"
        "y=3;"
        "f(20,5);"
        "sin(pi/2);"
        "DEG();"
        "sin(21);"
        "RAD();"
        "sin(21);"
        "GRAD();"
        "sin(21);"
        "28*ln(32)+sum(3,5,7,9,sin(pi/2),12);"
        "MODE();"
        "f(2,5);"
        "4%3;"
        "10%3;"
        "(13%9)+2^(5%3);"
        "-10%3;"
        "GRAD();"
        "sin(pi/2);"
        "g(x)=x^3-2*x+1;"
        "g(3);"
        "diff(g(x)-sin(x), x);"
        "diff(g(x),x, 3);"
        "diff(sin(x^3-3*x-2), x);"
        "diff(diff(sin(x^3), x), x);"
        "diff(diff(diff(sin(x), x), x), x);"
        "diff(sin(x), x, pi);";

    printf("Input program:\n%s\n\n", script);
    double result = exec(script);
    printf("\nLast evaluated value: %.17g\n", result);
    return 0;
}