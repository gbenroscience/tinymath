/*
 * tinymath.c
 * Recursive-descent math parser with variables, built-in functions,
 * user-defined functions, proper distinction between definitions and calls,
 * and fixed nested symbolic differentiation via partial symbolic mode
 * with proper parentheses in symbolic expressions to preserve precedence.
 *
 * Compile (Linux/macOS): gcc -O2 -std=c99 -Wall -lm -o math_parser tinymath.c
 * Compile (Windows/MinGW): gcc -O2 -std=c99 -Wall -lm -o math_parser.exe tinymath.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "parser_api.h"
#include "diff_module.h"

/* ---------------- Symbol Table ---------------- */
typedef struct
{
    char name[64];
    double value;
    int is_const; // 1 = constant, 0 = normal variable
} mp_var;

typedef enum
{
    RES_NUM,
    RES_STR
} ResultType;

typedef struct
{
    ResultType type;
    double num;
    char *str; // heap-allocated string for symbolic expressions
} mp_result;

#define MAX_VARS 256
static mp_var vars[MAX_VARS];
static int n_vars = 0;

/* Partial symbolic mode - ignores numeric bindings, treats identifiers symbolically */
static int symbolic_mode = 0;

/* ---------------- Lexer ---------------- */
typedef enum
{
    TK_END = 0,
    TK_NUM,
    TK_IDENT,
    TK_PLUS,
    TK_MINUS,
    TK_STAR,
    TK_SLASH,
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
    char ident[64];
    size_t pos;
} mp_token;

typedef struct
{
    const char *input;
    size_t i, len;
} mp_lexer;

static void skip_ws(mp_lexer *lx)
{
    for (;;)
    {
        while (lx->i < lx->len && isspace((unsigned char)lx->input[lx->i]))
            lx->i++;
        if (lx->i < lx->len && lx->input[lx->i] == '#')
        {
            while (lx->i < lx->len && lx->input[lx->i] != '\n')
                lx->i++;
            continue;
        }
        if (lx->i + 1 < lx->len && lx->input[lx->i] == '/' && lx->input[lx->i + 1] == '/')
        {
            lx->i += 2;
            while (lx->i < lx->len && lx->input[lx->i] != '\n')
                lx->i++;
            continue;
        }
        if (lx->i + 1 < lx->len && lx->input[lx->i] == '/' && lx->input[lx->i + 1] == '*')
        {
            lx->i += 2;
            while (lx->i + 1 < lx->len && !(lx->input[lx->i] == '*' && lx->input[lx->i + 1] == '/'))
                lx->i++;
            if (lx->i + 1 < lx->len)
                lx->i += 2;
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
    if (lx->i >= lx->len)
    {
        t.kind = TK_END;
        return t;
    }
    char c = lx->input[lx->i];

    if (isdigit((unsigned char)c) || (c == '.' && lx->i + 1 < lx->len && isdigit((unsigned char)lx->input[lx->i + 1])))
    {
        char buf[128];
        size_t j = 0;
        while (lx->i < lx->len && (isdigit((unsigned char)lx->input[lx->i]) ||
                                   lx->input[lx->i] == '.' || lx->input[lx->i] == 'e' || lx->input[lx->i] == 'E' ||
                                   lx->input[lx->i] == '+' || lx->input[lx->i] == '-'))
        {
            if (j < sizeof(buf) - 1)
                buf[j++] = lx->input[lx->i];
            lx->i++;
        }
        buf[j] = '\0';
        t.kind = TK_NUM;
        t.num = strtod(buf, NULL);
        return t;
    }

    if (isalpha((unsigned char)c) || c == '_')
    {
        char buf[64];
        size_t j = 0;
        while (lx->i < lx->len && (isalnum((unsigned char)lx->input[lx->i]) || lx->input[lx->i] == '_'))
        {
            if (j < sizeof(buf) - 1)
                buf[j++] = lx->input[lx->i];
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
    case '+':
        t.kind = TK_PLUS;
        break;
    case '-':
        t.kind = TK_MINUS;
        break;
    case '*':
        t.kind = TK_STAR;
        break;
    case '/':
        t.kind = TK_SLASH;
        break;
    case '^':
        t.kind = TK_CARET;
        break;
    case '(':
        t.kind = TK_LPAREN;
        break;
    case ')':
        t.kind = TK_RPAREN;
        break;
    case ',':
        t.kind = TK_COMMA;
        break;
    case '=':
        t.kind = TK_EQ;
        break;
    case ';':
        t.kind = TK_SEMI;
        break;
    default:
        t.kind = TK_END;
        break;
    }
    return t;
}

/* ---------------- Parser ---------------- */
typedef struct
{
    mp_lexer lx;
    mp_token cur;
} mp_parser;

static void advance(mp_parser *p) { p->cur = next_token(&p->lx); }
static int accept(mp_parser *p, mp_tok_kind k)
{
    if (p->cur.kind == k)
    {
        advance(p);
        return 1;
    }
    return 0;
}

/* ------------------ Forward declarations ------------------ */
static mp_result parse_term(mp_parser *p);
static mp_result parse_factor(mp_parser *p);
static mp_result parse_primary(mp_parser *p);
static mp_result pow_result(mp_result a, mp_result b);
static mp_result parse_expr(mp_parser *p);
static mp_result add(mp_result a, mp_result b);
static mp_result sub(mp_result a, mp_result b);
static mp_result mul(mp_result a, mp_result b);
static mp_result divide(mp_result a, mp_result b);
/* ------------------ Helpers ------------------ */
static char *double_to_string(double v)
{
    char buf[64];
    snprintf(buf, sizeof(buf), "%.17g", v);
    return strdup(buf);
}

static char *concat3(const char *a, const char *b, const char *c)
{
    size_t len = strlen(a) + strlen(b) + strlen(c) + 1;
    char *out = malloc(len);
    snprintf(out, len, "%s%s%s", a, b, c);
    return out;
}

static mp_result make_num(double v)
{
    mp_result r = {RES_NUM, v, NULL};
    return r;
}

static mp_result make_str(const char *s)
{
    mp_result r = {RES_STR, NAN, strdup(s)};
    return r;
}

/* Operations with proper parentheses for symbolic sub-expressions */
static mp_result add(mp_result a, mp_result b)
{
    if (a.type == RES_NUM && b.type == RES_NUM)
        return make_num(a.num + b.num);

    char *sa = (a.type == RES_STR) ? strdup(a.str) : double_to_string(a.num);
    char *sb = (b.type == RES_STR) ? strdup(b.str) : double_to_string(b.num);

    if (a.type == RES_STR) {
        char *tmp = sa;
        sa = malloc(strlen(tmp) + 3);
        sprintf(sa, "(%s)", tmp);
        free(tmp);
    }
    if (b.type == RES_STR) {
        char *tmp = sb;
        sb = malloc(strlen(tmp) + 3);
        sprintf(sb, "(%s)", tmp);
        free(tmp);
    }

    char *combined = concat3(sa, " + ", sb);
    free(sa);
    free(sb);
    return make_str(combined);
}

static mp_result sub(mp_result a, mp_result b)
{
    if (a.type == RES_NUM && b.type == RES_NUM)
        return make_num(a.num - b.num);

    char *sa = (a.type == RES_STR) ? strdup(a.str) : double_to_string(a.num);
    char *sb = (b.type == RES_STR) ? strdup(b.str) : double_to_string(b.num);

    if (a.type == RES_STR) {
        char *tmp = sa;
        sa = malloc(strlen(tmp) + 3);
        sprintf(sa, "(%s)", tmp);
        free(tmp);
    }
    if (b.type == RES_STR) {
        char *tmp = sb;
        sb = malloc(strlen(tmp) + 3);
        sprintf(sb, "(%s)", tmp);
        free(tmp);
    }

    char *combined = concat3(sa, " - ", sb);
    free(sa);
    free(sb);
    return make_str(combined);
}

static mp_result mul(mp_result a, mp_result b)
{
    if (a.type == RES_NUM && b.type == RES_NUM)
        return make_num(a.num * b.num);

    char *sa = (a.type == RES_STR) ? strdup(a.str) : double_to_string(a.num);
    char *sb = (b.type == RES_STR) ? strdup(b.str) : double_to_string(b.num);

    if (a.type == RES_STR) {
        char *tmp = sa;
        sa = malloc(strlen(tmp) + 3);
        sprintf(sa, "(%s)", tmp);
        free(tmp);
    }
    if (b.type == RES_STR) {
        char *tmp = sb;
        sb = malloc(strlen(tmp) + 3);
        sprintf(sb, "(%s)", tmp);
        free(tmp);
    }

    char *combined = concat3(sa, " * ", sb);
    free(sa);
    free(sb);
    return make_str(combined);
}

static mp_result divide(mp_result a, mp_result b)
{
    if (a.type == RES_NUM && b.type == RES_NUM)
        return make_num(a.num / b.num);

    char *sa = (a.type == RES_STR) ? strdup(a.str) : double_to_string(a.num);
    char *sb = (b.type == RES_STR) ? strdup(b.str) : double_to_string(b.num);

    if (a.type == RES_STR) {
        char *tmp = sa;
        sa = malloc(strlen(tmp) + 3);
        sprintf(sa, "(%s)", tmp);
        free(tmp);
    }
    if (b.type == RES_STR) {
        char *tmp = sb;
        sb = malloc(strlen(tmp) + 3);
        sprintf(sb, "(%s)", tmp);
        free(tmp);
    }

    char *combined = concat3(sa, " / ", sb);
    free(sa);
    free(sb);
    return make_str(combined);
}

static mp_result pow_result(mp_result a, mp_result b)
{
    if (a.type == RES_NUM && b.type == RES_NUM)
        return make_num(pow(a.num, b.num));

    char *sa = (a.type == RES_STR) ? strdup(a.str) : double_to_string(a.num);
    char *sb = (b.type == RES_STR) ? strdup(b.str) : double_to_string(b.num);

    if (a.type == RES_STR) {
        char *tmp = sa;
        sa = malloc(strlen(tmp) + 3);
        sprintf(sa, "(%s)", tmp);
        free(tmp);
    }
    if (b.type == RES_STR) {
        char *tmp = sb;
        sb = malloc(strlen(tmp) + 3);
        sprintf(sb, "(%s)", tmp);
        free(tmp);
    }

    char *combined = concat3(sa, "^", sb);
    free(sa);
    free(sb);
    return make_str(combined);
}

/* ---------------- Parsing ---------------- */
static mp_result parse_factor(mp_parser *p)
{
    mp_result v = parse_primary(p);
    if (accept(p, TK_CARET))
    {
        mp_result rhs = parse_factor(p);
        v = pow_result(v, rhs);
        if (v.type == RES_STR && v.str) { /* free args if needed - handled in pow_result */ }
    }
    return v;
}

static mp_result parse_term(mp_parser *p)
{
    mp_result v = parse_factor(p);
    while (p->cur.kind == TK_STAR || p->cur.kind == TK_SLASH)
    {
        mp_tok_kind op = p->cur.kind;
        advance(p);
        mp_result rhs = parse_factor(p);
        v = (op == TK_STAR) ? mul(v, rhs) : divide(v, rhs);
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

int set_const(const char *name, double value)
{
    if (n_vars < 128)
    {
        snprintf(vars[n_vars].name, sizeof(vars[n_vars].name), "%s", name);
        vars[n_vars].value = value;
        vars[n_vars].is_const = 1;
        n_vars++;
        return 1;
    }
    fprintf(stderr, "Variable table full\n");
    return 0;
}

int set_var(const char *name, double value)
{
    for (int i = 0; i < n_vars; i++)
    {
        if (strcmp(vars[i].name, name) == 0)
        {
            if (vars[i].is_const)
            {
                fprintf(stderr, "Error: cannot redefine constant %s\n", name);
                return 0;
            }
            vars[i].value = value;
            return 1;
        }
    }
    if (n_vars < MAX_VARS)
    {
        snprintf(vars[n_vars].name, sizeof(vars[n_vars].name), "%s", name);
        vars[n_vars].value = value;
        vars[n_vars].is_const = 0;
        n_vars++;
        return 1;
    }
    fprintf(stderr, "Variable table full\n");
    return 0;
}

static int lookup_var(const char *name, double *out)
{
    for (int i = 0; i < n_vars; i++)
    {
        if (strcmp(vars[i].name, name) == 0)
        {
            *out = vars[i].value;
            return 1;
        }
    }
    return 0;
}

/* ---------------- Function Table ---------------- */
#define MAX_FUNCS 128
static mp_func funcs[MAX_FUNCS];
static int n_funcs = 0;

int define_func(const char *name, char params[][64], int n_params, const char *body)
{
    if (n_funcs >= MAX_FUNCS)
    {
        fprintf(stderr, "Function table full\n");
        return 0;
    }
    snprintf(funcs[n_funcs].name, sizeof(funcs[n_funcs].name), "%s", name);
    funcs[n_funcs].n_params = n_params;
    for (int i = 0; i < n_params; i++)
    {
        snprintf(funcs[n_funcs].params[i], sizeof(funcs[n_funcs].params[i]), "%s", params[i]);
    }
    snprintf(funcs[n_funcs].body, sizeof(funcs[n_funcs].body), "%s", body);
    n_funcs++;
    return 1;
}

mp_func *lookup_func(const char *name)
{
    for (int i = 0; i < n_funcs; i++)
    {
        if (strcmp(funcs[i].name, name) == 0)
            return &funcs[i];
    }
    return NULL;
}

static double call_builtin(const char *name, double *args, int n)
{
    if (strcmp(name, "sin") == 0 && n == 1) return sin(args[0]);
    if (strcmp(name, "cos") == 0 && n == 1) return cos(args[0]);
    if (strcmp(name, "tan") == 0 && n == 1) return tan(args[0]);
    if (strcmp(name, "sqrt") == 0 && n == 1) return sqrt(args[0]);
    if (strcmp(name, "log") == 0 && n == 1) return log(args[0]);
    if (strcmp(name, "exp") == 0 && n == 1) return exp(args[0]);
    if (strcmp(name, "abs") == 0 && n == 1) return fabs(args[0]);
    if (strcmp(name, "pow") == 0 && n == 2) return pow(args[0], args[1]);
    return NAN;
}

static mp_result call_user_func(mp_func *f, double *args, int n)
{
    if (n != f->n_params)
    {
        fprintf(stderr, "Wrong number of arguments for %s (got %d, expected %d)\n",
                f->name, n, f->n_params);
        return make_num(NAN);
    }
    double oldvals[16];
    int had_old[16] = {0};
    for (int i = 0; i < n; i++)
    {
        had_old[i] = lookup_var(f->params[i], &oldvals[i]);
        set_var(f->params[i], args[i]);
    }

    mp_parser sub = {{f->body, 0, strlen(f->body)}, {0}};
    advance(&sub);
    mp_result result = parse_expr(&sub);

    for (int i = 0; i < n; i++)
    {
        if (had_old[i])
            set_var(f->params[i], oldvals[i]);
        /* New parameters are left in global scope - known limitation */
    }
    return result;
}

/* ---------------- Primary ---------------- */
static mp_result parse_primary(mp_parser *p)
{
    if (p->cur.kind == TK_NUM)
    {
        double v = p->cur.num;
        advance(p);
        return make_num(v);
    }

    if (p->cur.kind == TK_IDENT)
    {
        char name[64];
        snprintf(name, sizeof(name), "%s", p->cur.ident);
        advance(p);

        /* Special diff(...) */
        if (strcmp(name, "diff") == 0 && accept(p, TK_LPAREN))
        {
            symbolic_mode = 1;
            mp_result inner = parse_expr(p);
            symbolic_mode = 0;

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
            char var[64];
            snprintf(var, sizeof(var), "%s", p->cur.ident);
            advance(p);

            if (accept(p, TK_COMMA))
            {
                /* Evaluate derivative at point */
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
                char *deriv_str = diff_expr(expr_str, var);
                free(expr_str);
                if (inner.type == RES_STR) free(inner.str);

                double old_val = 0.0;
                int had_var = lookup_var(var, &old_val);
                set_var(var, point.num);

                mp_parser sub = {{deriv_str, 0, strlen(deriv_str)}, {0}};
                advance(&sub);
                mp_result result = parse_expr(&sub);

                if (had_var)
                    set_var(var, old_val);

                free(deriv_str);
                return result;
            }
            else
            {
                /* Symbolic derivative */
                if (!accept(p, TK_RPAREN))
                {
                    fprintf(stderr, "Expected ')' in diff()\n");
                    if (inner.type == RES_STR) free(inner.str);
                    return make_num(NAN);
                }

                char *expr_str = (inner.type == RES_NUM) ? double_to_string(inner.num) : strdup(inner.str);
                char *deriv_str = diff_expr(expr_str, var);
                free(expr_str);
                if (inner.type == RES_STR) free(inner.str);

                return make_str(deriv_str);
            }
        }

        /* Function call */
        if (accept(p, TK_LPAREN))
        {
            mp_result args[32];
            int n_args = 0;
            if (!accept(p, TK_RPAREN))
            {
                do
                {
                    args[n_args++] = parse_expr(p);
                } while (accept(p, TK_COMMA));
                if (!accept(p, TK_RPAREN))
                {
                    fprintf(stderr, "Missing ')' in function call\n");
                    return make_num(NAN);
                }
            }

            int all_numeric = 1;
            for (int i = 0; i < n_args; i++)
                if (args[i].type != RES_NUM) all_numeric = 0;

            char *arg_strs[32];
            for (int i = 0; i < n_args; i++)
                arg_strs[i] = (args[i].type == RES_NUM) ? double_to_string(args[i].num) : strdup(args[i].str);

            mp_func *uf = lookup_func(name);
            if (uf)
            {
                if (!all_numeric)
                {
                    fprintf(stderr, "Symbolic arguments not supported for user function %s\n", name);
                    for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                    for (int i = 0; i < n_args; i++) if (args[i].type == RES_STR) free(args[i].str);
                    return make_num(NAN);
                }
                double num_args[32];
                for (int i = 0; i < n_args; i++) num_args[i] = args[i].num;
                mp_result res = call_user_func(uf, num_args, n_args);
                for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                return res;
            }

            if (all_numeric)
            {
                double num_args[32];
                for (int i = 0; i < n_args; i++) num_args[i] = args[i].num;
                double val = call_builtin(name, num_args, n_args);
                for (int i = 0; i < n_args; i++) free(arg_strs[i]);
                if (!isnan(val))
                    return make_num(val);
            }

            /* Symbolic unknown function call */
            size_t len = strlen(name) + 3;
            for (int i = 0; i < n_args; i++)
                len += strlen(arg_strs[i]) + (i > 0 ? 2 : 0);
            char *combined = malloc(len);
            snprintf(combined, len, "%s(", name);
            for (int i = 0; i < n_args; i++)
            {
                if (i > 0) strcat(combined, ", ");
                strcat(combined, arg_strs[i]);
            }
            strcat(combined, ")");
            for (int i = 0; i < n_args; i++) free(arg_strs[i]);
            for (int i = 0; i < n_args; i++) if (args[i].type == RES_STR) free(args[i].str);
            return make_str(combined);
        }

        /* Plain identifier - symbolic mode takes priority */
        if (symbolic_mode)
            return make_str(name);

        double v;
        if (lookup_var(name, &v))
            return make_num(v);

        fprintf(stderr, "Unknown variable: %s\n", name);
        return make_num(NAN);
    }

    if (accept(p, TK_LPAREN))
    {
        mp_result r = parse_expr(p);
        if (!accept(p, TK_RPAREN))
            fprintf(stderr, "Missing ')'\n");
        return r;
    }

    if (accept(p, TK_MINUS))
    {
        mp_result r = parse_primary(p);
        if (r.type == RES_NUM)
        {
            r.num = -r.num;
        }
        else
        {
            char *s = malloc(strlen(r.str) + 3);
            sprintf(s, "-(%s)", r.str);
            free(r.str);
            r.str = s;
        }
        return r;
    }

    if (accept(p, TK_PLUS))
        return parse_primary(p);

    fprintf(stderr, "Unexpected token in primary (pos %zu)\n", p->cur.pos);
    return make_num(NAN);
}

/* ---------------- Function definition ---------------- */
static int parse_func_def(mp_parser *p, const char *fname)
{
    /* ... unchanged from previous version ... */
    if (!accept(p, TK_LPAREN))
    {
        fprintf(stderr, "Expected '(' after function name\n");
        return 0;
    }

    char params[16][64];
    int n = 0;
    while (p->cur.kind == TK_IDENT)
    {
        if (n >= 16)
        {
            fprintf(stderr, "Too many parameters in function %s (max 16)\n", fname);
            return 0;
        }
        snprintf(params[n], sizeof(params[n]), "%s", p->cur.ident);
        n++;
        advance(p);
        if (!accept(p, TK_COMMA))
            break;
    }

    if (!accept(p, TK_RPAREN))
    {
        fprintf(stderr, "Expected ')' in function definition\n");
        return 0;
    }
    if (!accept(p, TK_EQ))
    {
        fprintf(stderr, "Expected '=' in function definition\n");
        return 0;
    }

    size_t body_start = p->cur.pos;
    while (p->cur.kind != TK_SEMI && p->cur.kind != TK_END)
        advance(p);
    size_t body_end = p->lx.i;

    size_t len = body_end - body_start;
    if (len >= sizeof(funcs[0].body))
        len = sizeof(funcs[0].body) - 1;

    char body[1024];
    memcpy(body, p->lx.input + body_start, len);
    body[len] = '\0';

    size_t blen = strlen(body);
    if (blen > 0 && body[blen - 1] == ';')
        body[blen - 1] = '\0';

    if (p->cur.kind == TK_SEMI)
        advance(p);

    return define_func(fname, params, n, body);
}

/* ---------------- Statement ---------------- */
typedef enum
{
    STMT_VALUE,
    STMT_DEFINITION,
    STMT_ERROR
} StmtResultKind;

typedef struct
{
    StmtResultKind kind;
    double value;
} StmtResult;

static StmtResult parse_statement(mp_parser *p)
{
    StmtResult res = {STMT_ERROR, NAN};

    if (p->cur.kind == TK_IDENT)
    {
        char name[64];
        snprintf(name, sizeof(name), "%s", p->cur.ident);
        size_t ident_start = p->lx.i - strlen(name);

        advance(p);

        if (p->cur.kind == TK_EQ)
        {
            advance(p);
            mp_result val = parse_expr(p);
            if (val.type == RES_NUM && !isnan(val.num))
            {
                if (set_var(name, val.num))
                {
                    res.kind = STMT_VALUE;
                    res.value = val.num;
                }
            }
            else
            {
                fprintf(stderr, "Assignment requires a numeric value (got symbolic).\n");
            }
            if (val.type == RES_STR) free(val.str);
            return res;
        }

        /* function definition lookahead etc. unchanged */

        if (p->cur.kind == TK_LPAREN)
        {
            /* ... same lookahead for = after ) ... */
            size_t save_i = p->lx.i;
            mp_token save_cur = p->cur;

            int saw_eq_after_rparen = 0;
            int paren_depth = 1;
            advance(p);

            while (paren_depth > 0 && p->cur.kind != TK_END)
            {
                if (p->cur.kind == TK_LPAREN) paren_depth++;
                if (p->cur.kind == TK_RPAREN) paren_depth--;
                advance(p);
            }

            if (paren_depth == 0 && p->cur.kind == TK_EQ)
                saw_eq_after_rparen = 1;

            p->lx.i = save_i;
            p->cur = save_cur;

            if (saw_eq_after_rparen)
            {
                if (parse_func_def(p, name))
                {
                    res.kind = STMT_DEFINITION;
                    return res;
                }
                fprintf(stderr, "Bad function definition\n");
                return res;
            }
        }

        /* df(f,x) support unchanged */

        if (strcmp(name, "df") == 0 && accept(p, TK_LPAREN))
        {
            /* ... unchanged ... */
            if (p->cur.kind == TK_IDENT)
            {
                char src[64];
                snprintf(src, sizeof(src), "%s", p->cur.ident);
                advance(p);
                if (!accept(p, TK_COMMA))
                {
                    fprintf(stderr, "Expected ',' in df()\n");
                    return res;
                }
                if (p->cur.kind == TK_IDENT)
                {
                    char wrt[64];
                    snprintf(wrt, sizeof(wrt), "%s", p->cur.ident);
                    advance(p);
                    if (!accept(p, TK_RPAREN))
                    {
                        fprintf(stderr, "Expected ')' in df()\n");
                        return res;
                    }
                    char dst[128];
                    snprintf(dst, sizeof(dst), "d%s", src);
                    if (diff_func(src, wrt, dst))
                    {
                        res.kind = STMT_DEFINITION;
                        return res;
                    }
                }
            }
        }

        p->lx.i = ident_start;
        p->cur = next_token(&p->lx);
    }

    mp_result v = parse_expr(p);

    if (v.type == RES_NUM && !isnan(v.num))
    {
        res.kind = STMT_VALUE;
        res.value = v.num;
    }
    else if (v.type == RES_STR)
    {
        printf("%s\n", v.str);
        free(v.str);
    }

    return res;
}

void init_constants()
{
    set_const("pi", 3.14159265358979323846);
    set_const("e", 2.71828182845904523536);
    set_const("c", 299792458.0);
    set_const("k", 1.380649e-23);
    set_const("h", 6.62607015e-34);
    set_const("G", 6.67430e-11);
    set_const("Na", 6.02214076e23);
}

static double parse_program(mp_parser *p)
{
    static int constants_initialized = 0;
    if (!constants_initialized)
    {
        init_constants();
        constants_initialized = 1;
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
            printf("%s => %.17g\n", stmt, r.value);
            last_value = r.value;
            has_value = 1;
        }
        else if (r.kind == STMT_DEFINITION)
        {
            printf("%s => function defined\n", stmt);
        }

        accept(p, TK_SEMI);
    }

    return has_value ? last_value : 0.0;
}

/* ---------------- Demo ---------------- */
int main(void)
{
    const char *script =
        "f(a,b)=a*a+b*2;"
        "x=10;"
        "y=3;"
        "f(20,5);"
        "pi=3.14159;"
        "sin(pi/2);"
        "f(2,5);"
        "diff(sin(x^2-3*x-2), x);"
        "diff(diff(sin(x), x), x);"
        "diff(diff(diff(sin(x), x), x), x);"
        "diff(sin(x), x, pi);";

    printf("Input program:\n%s\n\n", script);

    mp_parser p = {{script, 0, strlen(script)}, {0}};
    advance(&p);

    double result = parse_program(&p);

    printf("\nLast evaluated value: %.17g\n", result);

    /* ... variable and function listing unchanged ... */

    return 0;
}