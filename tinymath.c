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
/* ---------------- Parser ---------------- */

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
    TK_PERCENT, // new: % operator
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

typedef struct
{
    mp_lexer lx;
    mp_token cur;
} mp_parser;
/* ---------------- Dynamic Variable Table ---------------- */
typedef struct
{
    char name[64];
    double value;
    int is_const; // 1 = constant, 0 = normal variable
} mp_var;

static mp_var *vars = NULL;
static size_t n_vars = 0;
static size_t vars_capacity = 0;

static int add_var(const char *name, double value, int is_const)
{
    /* Check for redefinition */
    for (size_t i = 0; i < n_vars; i++)
    {
        if (strcmp(vars[i].name, name) == 0)
        {
            fprintf(stderr, "Error: redefining %s\n", name);
            return 0;
        }
    }
    /* Grow if needed */
    if (n_vars >= vars_capacity)
    {
        size_t new_cap = vars_capacity ? vars_capacity * 2 : 64;
        mp_var *temp = realloc(vars, new_cap * sizeof(mp_var));
        if (!temp)
        {
            fprintf(stderr, "Memory allocation failed for variables\n");
            return 0;
        }
        vars = temp;
        vars_capacity = new_cap;
    }
    snprintf(vars[n_vars].name, sizeof(vars[n_vars].name), "%s", name);
    vars[n_vars].value = value;
    vars[n_vars].is_const = is_const;
    n_vars++;
    return 1;
}
 

 

int set_const(const char *name, double value)
{
    return add_var(name, value, 1);
}
 

// 2. Adjust add_var to only be used for "NEW" variables
// (Remove the "redefining" error check from inside add_var if you use it this way)
int set_var(const char *name, double value)
{
    for (size_t i = 0; i < n_vars; i++)
    {
        if (strcmp(vars[i].name, name) == 0)
        {
            if (vars[i].is_const)
            {
                fprintf(stderr, "Error: cannot assign to constant %s\n", name);
                return 0;
            }
            vars[i].value = value;
            return 1;
        }
    }
    return add_var(name, value, 0);
}
static int lookup_var(const char *name, double *out)
{
    for (size_t i = 0; i < n_vars; i++)
    {
        if (strcmp(vars[i].name, name) == 0)
        {
            *out = vars[i].value;
            return 1;
        }
    }
    return 0;
}

/* Partial symbolic mode - ignores numeric bindings, treats identifiers symbolically */
static int symbolic_mode = 0;

/* Trigonometric mode */
typedef enum
{
    MODE_RAD,
    MODE_DEG,
    MODE_GRAD
} trig_mode_t;
static trig_mode_t trig_mode = MODE_RAD;
static const double PI = 3.14159265358979323846;

static double to_radians(double angle)
{
    if (trig_mode == MODE_DEG)
        return angle * PI / 180.0;
    if (trig_mode == MODE_GRAD)
        return angle * PI / 200.0;
    return angle;
}

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

/* ------------------ Helpers ------------------ */

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
/* ---------------- Helpers for symbolic operations ---------------- */
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

/* ------------------ Forward declarations ------------------ */
static mp_result parse_term(mp_parser *p);
static mp_result parse_factor(mp_parser *p);
static mp_result parse_primary(mp_parser *p);
static mp_result pow_result(mp_result a, mp_result b);
static mp_result parse_expr(mp_parser *p);
/* Binary operations with proper parentheses for symbolic sub-expressions */
static mp_result add(mp_result a, mp_result b);
static mp_result sub(mp_result a, mp_result b);
static mp_result mul(mp_result a, mp_result b);
static mp_result divide(mp_result a, mp_result b);
static mp_result mod_result(mp_result a, mp_result b); // new: remainder
 
/* Universal macro for both operators and math functions */
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
        free(sa);                                                                 \
        free(sb);                                                                 \
        return make_str(combined);                                                \
    }

 

/* Standard Operators */
DEFINE_BINOP_EXT(add,    " + ", a.num + b.num)
DEFINE_BINOP_EXT(sub,    " - ", a.num - b.num)
DEFINE_BINOP_EXT(mul,    " * ", a.num * b.num)
DEFINE_BINOP_EXT(divide, " / ", a.num / b.num)

/* Functional Operators (Now safe to delete the manual ones) */
DEFINE_BINOP_EXT(mod_result, " % ", fmod(a.num, b.num))
DEFINE_BINOP_EXT(pow_result, "^",   pow(a.num, b.num))



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
    case '%':
        t.kind = TK_PERCENT;
        break; // new: % operator
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


/* ---------------- Parsing ---------------- */
static mp_result parse_factor(mp_parser *p)
{
    mp_result v = parse_primary(p);
    if (accept(p, TK_CARET))
    {
        mp_result rhs = parse_factor(p);
        v = pow_result(v, rhs);
        if (v.type == RES_STR && v.str)
        { /* free args if needed - handled in pow_result */
        }
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
        if (op == TK_STAR)
            v = mul(v, rhs);
        else if (op == TK_SLASH)
            v = divide(v, rhs);
        else
            v = mod_result(v, rhs); // new
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

/* ---------------- Function Table ---------------- */
static mp_func *funcs = NULL;     /* Pointer to dynamic array of functions */
static size_t n_funcs = 0;        /* Current number of defined functions */
static size_t funcs_capacity = 0; /* Current allocated capacity (for realloc growth) */
 

int define_func(const char *name, const char params[][MAX_IDENT_LEN], int n_params,
                const char *body_start, size_t body_len)
{
    if (n_params > 32) {
        fprintf(stderr, "Too many parameters (max 32)\n");
        return 0;
    }

    /* Make a copy of the body first */
    char *body_copy = malloc(body_len + 1);
    if (!body_copy) {
        fprintf(stderr, "Memory allocation failed for function body\n");
        return 0;
    }
    memcpy(body_copy, body_start, body_len);
    body_copy[body_len] = '\0';

    /* Ensure capacity for the funcs array */
    if (n_funcs >= funcs_capacity) {
        size_t new_cap = funcs_capacity ? funcs_capacity * 2 : 16;
        mp_func *temp = realloc(funcs, new_cap * sizeof(mp_func));
        if (!temp) {
            free(body_copy);
            fprintf(stderr, "Memory allocation failed for function table\n");
            return 0;
        }
        funcs = temp;
        funcs_capacity = new_cap;
    }

    /* Now append the new function */
    mp_func *f = &funcs[n_funcs++];
    snprintf(f->name, sizeof(f->name), "%s", name);
    f->n_params = n_params;
    for (int i = 0; i < n_params; i++) {
        snprintf(f->params[i], sizeof(f->params[i]), "%s", params[i]);
    }
    f->body = body_copy;
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

/* ---------------- Sorting helper ---------------- */
static int double_cmp(const void *aa, const void *bb)
{
    double a = *(const double *)aa;
    double b = *(const double *)bb;

    // 1. Handle NaN cases explicitly
    int a_nan = isnan(a);
    int b_nan = isnan(b);

    if (a_nan || b_nan)
    {
        // If both are NaN, they are "equal" for sorting purposes
        if (a_nan && b_nan)
            return 0;
        // If only A is NaN, A is "greater" (put it at the end)
        if (a_nan)
            return 1;
        // If only B is NaN, B is "greater" (A is smaller)
        return -1;
    }

    // 2. Standard comparison for valid numbers
    // Using the "Boolean Trick" from before
    return (a > b) - (a < b);
}

/* ---------------- Statistical functions (variable arity) ---------------- */
static double call_stat(const char *name, double *args, int n_args)
{
    if (n_args <= 0)
        return NAN;

    // 1. First Pass: O(n) Basic Stats
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

    // 2. Second Pass: O(n) Numerically Stable Variance
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
            return 0.0; // Avoid division by zero if all values are identical
        // We'll calculate the Z-score for the first value provided
        return (args[0] - mean) / std_pop;
    }

    // 3. Lazy Sorting (Only for Median/Mode)
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

    // 4. Return Logic (The New Stats)
    if (strcmp(name, "min") == 0)
        return min_v;
    if (strcmp(name, "max") == 0)
        return max_v;
    if (strcmp(name, "sum") == 0)
        return sum;
    if (strcmp(name, "mean") == 0 || strcmp(name, "avg") == 0)
        return mean;

    // --- NEW FUNCTIONS ---
    if (strcmp(name, "rng") == 0)
        return max_v - min_v;

    if (strcmp(name, "mrng") == 0)
        return (min_v + max_v) / 2.0;

    if (strcmp(name, "std_err") == 0 || strcmp(name, "sem") == 0)
        return std_sam / sqrt((double)n_args);
    // ---------------------

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

/* ---------------- Built-in functions ---------------- */
static double call_builtin(const char *name, double *args, int n)
{
    if (strcmp(name, "sin") == 0 && n == 1)
        return sin(to_radians(args[0]));
    if (strcmp(name, "cos") == 0 && n == 1)
        return cos(to_radians(args[0]));
    if (strcmp(name, "tan") == 0 && n == 1)
        return tan(to_radians(args[0]));
    if (strcmp(name, "sqrt") == 0 && n == 1)
        return sqrt(args[0]);
    if (strcmp(name, "log") == 0 && n == 1)
        return log(args[0]);
    if (strcmp(name, "exp") == 0 && n == 1)
        return exp(args[0]);
    if (strcmp(name, "abs") == 0 && n == 1)
        return fabs(args[0]);
    if (strcmp(name, "pow") == 0 && n == 2)
        return pow(args[0], args[1]);
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
    double oldvals[32];
    int had_old[32] = {0};
    for (int i = 0; i < n; i++)
    {
        had_old[i] = lookup_var(f->params[i], &oldvals[i]);
        set_var(f->params[i], args[i]);
    }

    // 1. Initialize the sub-parser
    mp_parser sub;
    memset(&sub, 0, sizeof(sub)); // Zero out everything
    
    sub.lx.input = f->body;
    sub.lx.i = 0;
    sub.lx.len = strlen(f->body);
    
    // 2. Priming the pump
    advance(&sub); // This loads the first token from the body into sub.cur
    
    // 3. Parse the expression
    mp_result result = parse_expr(&sub);

    for (int i = 0; i < n; i++)
    {
        if (had_old[i])
            set_var(f->params[i], oldvals[i]);
        /* New parameters are left in global scope - known limitation */
    }
    return result;
}
// static mp_result call_user_func(mp_func *f, double *args, int n)
// {
//     if (n != f->n_params)
//     {
//         fprintf(stderr, "Wrong number of arguments for %s (got %d, expected %d)\n",
//                 f->name, n, f->n_params);
//         return make_num(NAN);
//     }
//     double oldvals[32];
//     int had_old[32] = {0};
//     for (int i = 0; i < n; i++)
//     {
//         had_old[i] = lookup_var(f->params[i], &oldvals[i]);
//         set_var(f->params[i], args[i]);
//     }

//     mp_parser sub = {{f->body, 0, strlen(f->body)}, {0}};
//     advance(&sub);
//     mp_result result = parse_expr(&sub);

//     for (int i = 0; i < n; i++)
//     {
//         if (had_old[i])
//             set_var(f->params[i], oldvals[i]);
//         /* New parameters are left in global scope - known limitation */
//     }
//     return result;
// }

/* ---------------- Primary ---------------- */
/* Primary expression - main change in function call handling */
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
                if (inner.type == RES_STR)
                    free(inner.str);
                return make_num(NAN);
            }

            if (p->cur.kind != TK_IDENT)
            {
                fprintf(stderr, "Expected variable name after ',' in diff()\n");
                if (inner.type == RES_STR)
                    free(inner.str);
                return make_num(NAN);
            }
            char var[64];
            snprintf(var, sizeof(var), "%s", p->cur.ident);
            advance(p);

            if (accept(p, TK_COMMA))
            {
                /* numeric evaluation at point */
                mp_result point = parse_expr(p);
                if (!accept(p, TK_RPAREN))
                {
                    fprintf(stderr, "Expected ')' in diff()\n");
                    if (inner.type == RES_STR)
                        free(inner.str);
                    if (point.type == RES_STR)
                        free(point.str);
                    return make_num(NAN);
                }
                if (point.type != RES_NUM)
                {
                    fprintf(stderr, "Evaluation point must be numeric\n");
                    if (inner.type == RES_STR)
                        free(inner.str);
                    if (point.type == RES_STR)
                        free(point.str);
                    return make_num(NAN);
                }

                char *expr_str = (inner.type == RES_NUM) ? double_to_string(inner.num) : strdup(inner.str);
                char *deriv_str = diff_expr(expr_str, var);
                free(expr_str);
                if (inner.type == RES_STR)
                    free(inner.str);

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
                /* symbolic */
                if (!accept(p, TK_RPAREN))
                {
                    fprintf(stderr, "Expected ')' in diff()\n");
                    if (inner.type == RES_STR)
                        free(inner.str);
                    return make_num(NAN);
                }

                char *expr_str = (inner.type == RES_NUM) ? double_to_string(inner.num) : strdup(inner.str);
                char *deriv_str = diff_expr(expr_str, var);
                free(expr_str);
                if (inner.type == RES_STR)
                    free(inner.str);

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
                if (args[i].type != RES_NUM)
                    all_numeric = 0;

            /* ----- Trig mode functions (zero-arg) ----- */
            if (n_args == 0 && all_numeric)
            {
                if (strcmp(name, "DEG") == 0)
                {
                    trig_mode = MODE_DEG;
                    printf("Trigonometric mode set to Degrees\n");
                    return make_num(NAN); // suppress "=> NaN"
                }
                if (strcmp(name, "RAD") == 0)
                {
                    trig_mode = MODE_RAD;
                    printf("Trigonometric mode set to Radians\n");
                    return make_num(NAN);
                }
                if (strcmp(name, "GRAD") == 0)
                {
                    trig_mode = MODE_GRAD;
                    printf("Trigonometric mode set to Gradians\n");
                    return make_num(NAN);
                }
                if (strcmp(name, "MODE") == 0)
                {
                    const char *mstr = (trig_mode == MODE_DEG)    ? "Degrees"
                                       : (trig_mode == MODE_GRAD) ? "Gradians"
                                                                  : "Radians";
                    printf("Current trigonometric mode: %s\n", mstr);
                    return make_num(NAN);
                }
            }

            /* Normal function handling */
            char *arg_strs[32];
            for (int i = 0; i < n_args; i++)
                arg_strs[i] = (args[i].type == RES_NUM) ? double_to_string(args[i].num) : strdup(args[i].str);

            mp_func *uf = lookup_func(name);
            if (uf)
            {
                if (!all_numeric)
                {
                    fprintf(stderr, "Symbolic arguments not supported for user function %s\n", name);
                    for (int i = 0; i < n_args; i++)
                    {
                        free(arg_strs[i]);
                        if (args[i].type == RES_STR)
                            free(args[i].str);
                    }
                    return make_num(NAN);
                }
                double num_args[32];
                for (int i = 0; i < n_args; i++)
                    num_args[i] = args[i].num;
                mp_result res = call_user_func(uf, num_args, n_args);
                for (int i = 0; i < n_args; i++)
                    free(arg_strs[i]);
                return res;
            }

            if (all_numeric)
            {
                double num_args[32];
                for (int i = 0; i < n_args; i++)
                    num_args[i] = args[i].num;

                double val = call_builtin(name, num_args, n_args);
                if (isnan(val))
                    val = call_stat(name, num_args, n_args);

                for (int i = 0; i < n_args; i++)
                    free(arg_strs[i]);

                if (!isnan(val))
                    return make_num(val);
            }

            /* Symbolic fallback */
            size_t len = strlen(name) + 3;
            for (int i = 0; i < n_args; i++)
                len += strlen(arg_strs[i]) + (i > 0 ? 2 : 0);
            char *combined = malloc(len);
            snprintf(combined, len, "%s(", name);
            for (int i = 0; i < n_args; i++)
            {
                if (i > 0)
                    strcat(combined, ", ");
                strcat(combined, arg_strs[i]);
            }
            strcat(combined, ")");
            for (int i = 0; i < n_args; i++)
            {
                free(arg_strs[i]);
                if (args[i].type == RES_STR)
                    free(args[i].str);
            }
            return make_str(combined);
        }

        /* Plain identifier */
        if (symbolic_mode)
            return make_str(name);

        double v;
        if (lookup_var(name, &v))
            return make_num(v);

        fprintf(stderr, "Unknown variable: %s\n", name);
        return make_num(NAN);
    } 

    /* ... rest of primary (parens, unary -, +) unchanged ... */
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
            r.num = -r.num;
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

    fprintf(stderr, "Unexpected token in primary (pos %zu)\n", p->lx.i);
    return make_num(NAN);
}

/* ---------------- Function definition ---------------- */
static int parse_func_def(mp_parser *p, const char *fname)
{
    // 1. Check for opening parenthesis
    if (!accept(p, TK_LPAREN))
    {
        fprintf(stderr, "Error: Expected '(' after function name '%s'\n", fname);
        return 0;
    }

    char params[32][64];
    int n_params = 0;

    // 2. Parse Parameters strictly: f(a, b, c)
    while (p->cur.kind == TK_IDENT)
    {
        if (n_params >= 32)
        {
            fprintf(stderr, "Error: Function '%s' exceeds limit of 32 parameters\n", fname);
            return 0;
        }

        snprintf(params[n_params++], 64, "%s", p->cur.ident);
        advance(p);

        // If there's no comma, we expect the loop to end and see a ')'
        if (p->cur.kind == TK_COMMA)
        {
            advance(p);
            // After a comma, there MUST be another identifier
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

    // 3. Closing syntax checks
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

    // 4. Zero-Copy Body Capture
    // We point directly to the start of the expression in the input string
    const char *body_start = p->lx.input + p->lx.i;

    // Advance the parser until the end of the statement (semicolon or EOF)
    while (p->cur.kind != TK_SEMI && p->cur.kind != TK_END)
    {
        advance(p);
    }

    // Calculate length based on current lexer position
    size_t body_len = (p->lx.input + p->lx.i) - body_start;

    // Move past the semicolon if it exists
    if (p->cur.kind == TK_SEMI)
    {
        advance(p);
    }

    // 5. Pass to function manager
    // Ensure your define_func accepts (name, params, count, body_ptr, body_len)
    return define_func(fname, params, n_params, body_start, body_len);
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

static StmtResult parse_statement(mp_parser *p) {
    StmtResult res = {STMT_ERROR, NAN};

    /* Save FULL lexer state: position AND current token */
    size_t save_i = p->lx.i;
    mp_token save_cur = p->cur;

    if (p->cur.kind == TK_IDENT) {
        char name[64];
        snprintf(name, sizeof(name), "%s", p->cur.ident);

        /* Consume the identifier for lookahead */
        advance(p);

        /* Assignment: IDENT = expr */
        if (p->cur.kind == TK_EQ) {
            advance(p);  /* consume '=' */
            mp_result val = parse_expr(p);
            if (val.type == RES_NUM && !isnan(val.num)) {
                if (set_var(name, val.num)) {
                    res.kind = STMT_VALUE;
                    res.value = val.num;
                }
            } else {
                fprintf(stderr, "Assignment requires a numeric value.\n");
            }
            if (val.type == RES_STR) free(val.str);
            return res;
        }

        /* Function definition: IDENT ( ... ) = ... */
        if (p->cur.kind == TK_LPAREN) {
            size_t def_save_i = p->lx.i;
            mp_token def_save_cur = p->cur;

            int paren_depth = 1;
            advance(p);  /* consume '(' */
            while (paren_depth > 0 && p->cur.kind != TK_END) {
                if (p->cur.kind == TK_LPAREN) paren_depth++;
                if (p->cur.kind == TK_RPAREN) paren_depth--;
                advance(p);
            }

            int is_definition = (paren_depth == 0 && p->cur.kind == TK_EQ);

            /* Restore after lookahead */
            p->lx.i = def_save_i;
            p->cur = def_save_cur;

            if (is_definition) {
                if (parse_func_def(p, name)) {
                    res.kind = STMT_DEFINITION;
                    return res;
                }
                fprintf(stderr, "Invalid function definition for %s\n", name);
                return res;
            }
        }

        /* df(f,x) at statement level - place before rewind if you use it */
        /* (your df block here, using 'name' and checking p->cur.kind == TK_LPAREN) */
        	
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

        /* Not assignment or definition → plain expression (call, variable, etc.) */
        /* FULL RESTORE: position + current token */
        p->lx.i = save_i;
        p->cur = save_cur;
    }

    /* Parse the expression (now starts correctly with IDENT or other token) */
    mp_result v = parse_expr(p);

    if (v.type == RES_NUM && !isnan(v.num)) {
        res.kind = STMT_VALUE;
        res.value = v.num;
    } else if (v.type == RES_STR) {
        printf("%s\n", v.str);
        free(v.str);
    }

    /* Optional recovery on error */
    if (isnan(v.num) || (v.type == RES_STR && !v.str)) {
        while (p->cur.kind != TK_SEMI && p->cur.kind != TK_END) advance(p);
        if (p->cur.kind == TK_SEMI) advance(p);
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
        if (len >= sizeof(stmt))
            len = sizeof(stmt) - 1;
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

 

/* ---------------- Cleanup & exec ---------------- */
double exec(const char *script)
{
    mp_parser p = {{script, 0, strlen(script)}, {0}};
    advance(&p);
    double last_value = parse_program(&p);

    /* Cleanup */
    for (size_t i = 0; i < n_funcs; i++)
        free(funcs[i].body);
    free(funcs);
    funcs = NULL;
    n_funcs = 0;
    funcs_capacity = 0;

    free(vars);
    vars = NULL;
    n_vars = 0;
    vars_capacity = 0;

    return last_value;
}

/* ---------------- Demo ---------------- */
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
        "MODE();"
        "f(2,5);"
        "sum(3,5,7,9,sin(pi/2),12);"
        "4%3;"
        "10%3;"
        "(13%9)+2^(5%3);"
        "-10%3;"
        "GRAD();"
        "sin(pi/2);"
        "diff(sin(x^2-3*x-2), x);"
        "diff(diff(sin(x), x), x);"
        "diff(diff(diff(sin(x), x), x), x);"
        "diff(sin(x), x, pi);";

    printf("Input program:\n%s\n\n", script);
    double result = exec(script);
    printf("\nLast evaluated value: %.17g\n", result);
    return 0;
}