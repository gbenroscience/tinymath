/*
 * math_parser_full_fixed.c
 * Recursive-descent math parser with variables, built-in functions,
 * user-defined functions, and proper distinction between definitions and calls.
 *
 * Compile (Linux/macOS): gcc -O2 -std=c99 -Wall -lm -o math_parser math_parser_full_fixed.c
 * Compile (Windows/MinGW): gcc -O2 -std=c99 -Wall -lm -o math_parser.exe math_parser_full_fixed.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "parser_api.h"
#include "diff_module.h"



/* ---------------- Symbol Table ---------------- */
typedef struct {
    char name[64];
    double value;
    int is_const; // 1 = constant, 0 = normal variable
} mp_var;

 
#define MAX_VARS 256
static mp_var vars[MAX_VARS];
static int n_vars = 0;

int set_const(const char *name, double value) {
    if (n_vars < 128) {
        snprintf(vars[n_vars].name, sizeof(vars[n_vars].name), "%s", name);
        vars[n_vars].value = value;
        vars[n_vars].is_const = 1;  // mark as constant
        n_vars++;
        return 1;
    }
    fprintf(stderr, "Variable table full\n");
    return 0;
}

int set_var(const char *name, double value) {
    for (int i = 0; i < n_vars; i++) {
        if (strcmp(vars[i].name, name) == 0) {
            if (vars[i].is_const) {
                fprintf(stderr, "Error: cannot redefine constant %s\n", name);
                return 0;
            }
            vars[i].value = value;
            return 1;
        }
    }
    if (n_vars < MAX_VARS) {
        snprintf(vars[n_vars].name, sizeof(vars[n_vars].name), "%s", name);
        vars[n_vars].value = value;
        vars[n_vars].is_const = 0;  // default: normal variable
        n_vars++;
        return 1;
    }
    fprintf(stderr, "Variable table full\n");
    return 0;
}

static int lookup_var(const char *name, double *out) {
    for (int i = 0; i < n_vars; i++) {
        if (strcmp(vars[i].name, name) == 0) {
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

 int define_func(const char *name, char params[][64], int n_params, const char *body) {
    if (n_funcs >= MAX_FUNCS) {
        fprintf(stderr, "Function table full\n");
        return 0;
    }
    snprintf(funcs[n_funcs].name, sizeof(funcs[n_funcs].name), "%s", name);
    funcs[n_funcs].n_params = n_params;
    for (int i = 0; i < n_params; i++) {
        snprintf(funcs[n_funcs].params[i], sizeof(funcs[n_funcs].params[i]), "%s", params[i]);
    }
    snprintf(funcs[n_funcs].body, sizeof(funcs[n_funcs].body), "%s", body);
    n_funcs++;
    return 1;
}

 mp_func* lookup_func(const char *name) {
    for (int i = 0; i < n_funcs; i++) {
        if (strcmp(funcs[i].name, name) == 0) return &funcs[i];
    }
    return NULL;
}

/* ---------------- Lexer ---------------- */
typedef enum {
    TK_END = 0, TK_NUM, TK_IDENT,
    TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH, TK_CARET,
    TK_LPAREN, TK_RPAREN, TK_COMMA, TK_EQ, TK_SEMI
} mp_tok_kind;

typedef struct {
    mp_tok_kind kind;
    double num;
    char ident[64];
    size_t pos;
} mp_token;

typedef struct {
    const char *input;
    size_t i, len;
} mp_lexer;

static void skip_ws(mp_lexer *lx) {
    for (;;) {
        while (lx->i < lx->len && isspace((unsigned char)lx->input[lx->i])) lx->i++;
        if (lx->i < lx->len && lx->input[lx->i] == '#') {
            while (lx->i < lx->len && lx->input[lx->i] != '\n') lx->i++;
            continue;
        }
        if (lx->i+1 < lx->len && lx->input[lx->i] == '/' && lx->input[lx->i+1] == '/') {
            lx->i += 2;
            while (lx->i < lx->len && lx->input[lx->i] != '\n') lx->i++;
            continue;
        }
        if (lx->i+1 < lx->len && lx->input[lx->i] == '/' && lx->input[lx->i+1] == '*') {
            lx->i += 2;
            while (lx->i+1 < lx->len && !(lx->input[lx->i] == '*' && lx->input[lx->i+1] == '/')) lx->i++;
            if (lx->i+1 < lx->len) lx->i += 2;
            continue;
        }
        break;
    }
}

static mp_token next_token(mp_lexer *lx) {
    skip_ws(lx);
    mp_token t = {0};
    t.pos = lx->i;
    if (lx->i >= lx->len) { t.kind = TK_END; return t; }
    char c = lx->input[lx->i];

    if (isdigit((unsigned char)c) || (c == '.' && lx->i+1 < lx->len && isdigit((unsigned char)lx->input[lx->i+1]))) {
        char buf[128]; size_t j = 0;
        while (lx->i < lx->len && (isdigit((unsigned char)lx->input[lx->i]) ||
               lx->input[lx->i] == '.' || lx->input[lx->i] == 'e' || lx->input[lx->i] == 'E' ||
               lx->input[lx->i] == '+' || lx->input[lx->i] == '-')) {
            if (j < sizeof(buf)-1) buf[j++] = lx->input[lx->i];
            lx->i++;
        }
        buf[j] = '\0';
        t.kind = TK_NUM; t.num = strtod(buf, NULL);
        return t;
    }

    if (isalpha((unsigned char)c) || c == '_') {
        char buf[64]; size_t j = 0;
        while (lx->i < lx->len && (isalnum((unsigned char)lx->input[lx->i]) || lx->input[lx->i] == '_')) {
            if (j < sizeof(buf)-1) buf[j++] = lx->input[lx->i];
            lx->i++;
        }
        buf[j] = '\0';
        t.kind = TK_IDENT;
        snprintf(t.ident, sizeof(t.ident), "%s", buf);
        return t;
    }

    lx->i++;
    switch (c) {
        case '+': t.kind = TK_PLUS; break;
        case '-': t.kind = TK_MINUS; break;
        case '*': t.kind = TK_STAR; break;
        case '/': t.kind = TK_SLASH; break;
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

/* ---------------- Parser ---------------- */
typedef struct {
    mp_lexer lx;
    mp_token cur;
} mp_parser;

static void advance(mp_parser *p) { p->cur = next_token(&p->lx); }
static int accept(mp_parser *p, mp_tok_kind k) {
    if (p->cur.kind == k) { advance(p); return 1; }
    return 0;
}

static double parse_expr(mp_parser *p);  /* forward */

/* Built-in functions */
static double call_builtin(const char *name, double *args, int n) {
    if (strcmp(name, "sin") == 0  && n == 1) return sin(args[0]);
    if (strcmp(name, "cos") == 0  && n == 1) return cos(args[0]);
    if (strcmp(name, "tan") == 0  && n == 1) return tan(args[0]);
    if (strcmp(name, "sqrt")== 0  && n == 1) return sqrt(args[0]);
    if (strcmp(name, "log") == 0  && n == 1) return log(args[0]);
    if (strcmp(name, "exp") == 0  && n == 1) return exp(args[0]);
    if (strcmp(name, "abs") == 0  && n == 1) return fabs(args[0]);
    if (strcmp(name, "pow") == 0  && n == 2) return pow(args[0], args[1]);
    return NAN;
}

static double call_user_func(mp_func *f, double *args, int n) {
    if (n != f->n_params) {
        fprintf(stderr, "Wrong number of arguments for %s (got %d, expected %d)\n",
                f->name, n, f->n_params);
        return NAN;
    }
    double oldvals[16];
    int had_old[16] = {0};
    for (int i = 0; i < n; i++) {
        had_old[i] = lookup_var(f->params[i], &oldvals[i]);
        set_var(f->params[i], args[i]);
    }

    mp_parser sub = {{f->body, 0, strlen(f->body)}, {0}};
    advance(&sub);
    double result = parse_expr(&sub);

    for (int i = 0; i < n; i++) {
        if (had_old[i]) {
            set_var(f->params[i], oldvals[i]);
        }
        // If not previously defined, do nothing — avoids polluting global vars
    }
    return result;
}


/* ------------------ Expression parsing ------------------ */
static double parse_primary(mp_parser *p) {
    if (p->cur.kind == TK_NUM) {
        double v = p->cur.num;
        advance(p);
        return v;
    }
    if (p->cur.kind == TK_IDENT) {
        char name[64];
        snprintf(name, sizeof(name), "%s", p->cur.ident);
        advance(p);
        // Handle diff(...)
if (strcmp(name, "diff") == 0 && accept(p, TK_LPAREN)) {
    // First argument: expression substring
    size_t expr_start = p->cur.pos;
    parse_expr(p); // consume expression
    size_t expr_end = p->lx.i;

    size_t len = expr_end - expr_start;
    char expr_buf[1024];
    if (len >= sizeof(expr_buf)) len = sizeof(expr_buf)-1;
    memcpy(expr_buf, p->lx.input + expr_start, len);
    expr_buf[len] = '\0';

    if (!accept(p, TK_COMMA)) {
        fprintf(stderr, "Expected ',' in diff()\n");
        return NAN;
    }

    // Second argument: variable name
    if (p->cur.kind != TK_IDENT) {
        fprintf(stderr, "Expected variable in diff()\n");
        return NAN;
    }
    char var[64];
    snprintf(var, sizeof(var), "%s", p->cur.ident);
    advance(p);

    double eval_value = NAN;
    int has_third_arg = 0;

    // Optional third argument
    if (accept(p, TK_COMMA)) {
        eval_value = parse_expr(p);
        has_third_arg = 1;
    }

    if (!accept(p, TK_RPAREN)) {
        fprintf(stderr, "Expected ')' in diff()\n");
        return NAN;
    }

    // Get symbolic derivative
    char* dbody = diff_expr(expr_buf, var);

    if (!has_third_arg) {
        // Overload 1: return symbolic derivative as a function string
        printf("Derivative: %s\n", dbody);
        // Define temporary function diff_tmp(var) = dbody
        char params[1][64];
        snprintf(params[0], sizeof(params[0]), "%s", var);
        define_func("diff_tmp", params, 1, dbody);
        free(dbody);
        return NAN; // or some sentinel, since symbolic only
    } else {
        // Overload 2: evaluate derivative at given value
        char params[1][64];
        snprintf(params[0], sizeof(params[0]), "%s", var);
        define_func("diff_tmp", params, 1, dbody);
        free(dbody);

        double args[1] = {eval_value};
        mp_func* f = lookup_func("diff_tmp");
        return call_user_func(f, args, 1);
    }
}

        if (accept(p, TK_LPAREN)) {
            double args[32]; int n = 0;
            if (!accept(p, TK_RPAREN)) {
                do {
                    args[n++] = parse_expr(p);
                } while (accept(p, TK_COMMA));
                if (!accept(p, TK_RPAREN)) {
                    fprintf(stderr, "Missing ')' in function call\n");
                    return NAN;
                }
            }
            mp_func *uf = lookup_func(name);
            if (uf) return call_user_func(uf, args, n);
            return call_builtin(name, args, n);
        }
        double v;
        if (lookup_var(name, &v)) return v;
        fprintf(stderr, "Unknown variable or function: %s\n", name);
        return NAN;
    }
    if (accept(p, TK_LPAREN)) {
        double v = parse_expr(p);
        if (!accept(p, TK_RPAREN)) fprintf(stderr, "Missing ')'\n");
        return v;
    }
    if (accept(p, TK_MINUS)) return -parse_primary(p);
    if (accept(p, TK_PLUS))  return  parse_primary(p);

    fprintf(stderr, "Unexpected token in primary (pos %zu)\n", p->cur.pos);
    return NAN;
}

static double parse_factor(mp_parser *p) {
    double v = parse_primary(p);
    if (accept(p, TK_CARET)) {
        double rhs = parse_factor(p);  // right-associative
        v = pow(v, rhs);
    }
    return v;
}

static double parse_term(mp_parser *p) {
    double v = parse_factor(p);
    while (p->cur.kind == TK_STAR || p->cur.kind == TK_SLASH) {
        mp_tok_kind op = p->cur.kind; advance(p);
        double rhs = parse_factor(p);
        if (op == TK_STAR) v *= rhs; else v /= rhs;
    }
    return v;
}

static double parse_expr(mp_parser *p) {
    double v = parse_term(p);
    while (p->cur.kind == TK_PLUS || p->cur.kind == TK_MINUS) {
        mp_tok_kind op = p->cur.kind; advance(p);
        double rhs = parse_term(p);
        if (op == TK_PLUS) v += rhs; else v -= rhs;
    }
    return v;
}

/* ------------------ Function definition ------------------ */ 
static int parse_func_def(mp_parser *p, const char *fname) {
    if (!accept(p, TK_LPAREN)) return 0;

    char params[16][64]; 
    int n = 0;
    while (p->cur.kind == TK_IDENT) {
        snprintf(params[n], sizeof(params[n]), "%s", p->cur.ident);
        n++;
        advance(p);
        if (!accept(p, TK_COMMA)) break;
    }

    if (!accept(p, TK_RPAREN)) {
        fprintf(stderr, "Expected ')' in function definition\n");
        return 0;
    }
    if (!accept(p, TK_EQ)) {
        fprintf(stderr, "Expected '=' in function definition\n");
        return 0;
    }

    /* Capture body text correctly */
    size_t body_start = p->cur.pos;
    while (p->cur.kind != TK_SEMI && p->cur.kind != TK_END) {
        advance(p);
    }
    size_t body_end = p->lx.i;

    size_t len = body_end - body_start;
    if (len >= sizeof(funcs[0].body)) len = sizeof(funcs[0].body) - 1;

    char body[1024];
    memcpy(body, p->lx.input + body_start, len);
    body[len] = '\0';

    /* Strip trailing semicolon if present */
    size_t blen = strlen(body);
    if (blen > 0 && body[blen-1] == ';') {
        body[blen-1] = '\0';
    }

    if (p->cur.kind == TK_SEMI) advance(p);

    return define_func(fname, params, n, body);
}

 

/* ------------------ Statement result type ------------------ */
typedef enum {
    STMT_VALUE,
    STMT_DEFINITION,
    STMT_ERROR
} StmtResultKind;

typedef struct {
    StmtResultKind kind;
    double value;
} StmtResult;

/* ------------------ Statement ------------------ */
static StmtResult parse_statement(mp_parser *p) {
    StmtResult res = {STMT_ERROR, NAN};

    if (p->cur.kind == TK_IDENT) {
        char name[64];
        snprintf(name, sizeof(name), "%s", p->cur.ident);
        size_t ident_start = p->lx.i - strlen(name);  // approximate

        advance(p);

        if (p->cur.kind == TK_EQ) {
            advance(p);
            double val = parse_expr(p);
            if (!isnan(val)) {
                set_var(name, val);
                res.kind = STMT_VALUE;
                res.value = val;
            }
            return res;
        }

        if (p->cur.kind == TK_LPAREN) {
            // possible function call OR definition
            size_t save_i = p->lx.i;
            mp_token save_cur = p->cur;

            // lookahead for =
            int saw_eq_after_rparen = 0;
            int paren_depth = 1;
            advance(p);  // eat initial '('

            while (paren_depth > 0 && p->cur.kind != TK_END) {
                if (p->cur.kind == TK_LPAREN) paren_depth++;
                if (p->cur.kind == TK_RPAREN) paren_depth--;
                advance(p);
            }

            if (paren_depth == 0 && p->cur.kind == TK_EQ) {
                saw_eq_after_rparen = 1;
            }

            p->lx.i = save_i;
            p->cur = save_cur;

            if (saw_eq_after_rparen) {
                if (parse_func_def(p, name)) {
                    res.kind = STMT_DEFINITION;
                    return res;
                }
                fprintf(stderr, "Bad function definition\n");
                return res;
            }
        }

    // Handle differentiation: df(f,x) defines derivative of f wrt x
    if (strcmp(name, "df") == 0 && accept(p, TK_LPAREN)) {
        // Expect function name and variable
        if (p->cur.kind == TK_IDENT) {
            char src[64]; snprintf(src, sizeof(src), "%s", p->cur.ident);
            advance(p);
            if (!accept(p, TK_COMMA)) {
                fprintf(stderr, "Expected ',' in df()\n");
                return res;
            }
            if (p->cur.kind == TK_IDENT) {
                char wrt[64]; snprintf(wrt, sizeof(wrt), "%s", p->cur.ident);
                advance(p);
                if (!accept(p, TK_RPAREN)) {
                    fprintf(stderr, "Expected ')' in df()\n");
                    return res;
                }
                // Define derivative function as "d<func>"
                char dst[128]; snprintf(dst, sizeof(dst), "d%s", src);
                if (diff_func(src, wrt, dst)) {
                    res.kind = STMT_DEFINITION;
                    return res;
                }
            }
        }
    }

        // If we reach here → it's either variable read or function call
        // → rewind to start of name and parse as expression
        p->lx.i = ident_start;
        p->cur = next_token(&p->lx);
    }

    // Parse any expression (covers function calls, variables, numbers, etc.)
    double v = parse_expr(p);
    if (!isnan(v)) {
        res.kind = STMT_VALUE;
        res.value = v;
    }

    return res;
}
/* Define constants at max double precision */
void init_constants() {
    set_const("pi", 3.14159265358979323846);          // π
    set_const("e", 2.71828182845904523536);           // Napier’s constant
    set_const("c", 299792458.0);                      // speed of light (m/s)
    set_const("k", 1.380649e-23);                     // Boltzmann constant (J/K)
    set_const("h", 6.62607015e-34);                   // Planck constant (J·s)
    set_const("G", 6.67430e-11);                      // Gravitational constant (m^3/kg/s^2)
    set_const("Na", 6.02214076e23);                   // Avogadro’s number (1/mol)
}

/* ------------------ Program ------------------ */
static double parse_program(mp_parser *p) {
   static int constants_initialized = 0;
    if (!constants_initialized) {
        init_constants();          // load constants once
        constants_initialized = 1; // prevent re‑adding on every run
    }
 

    double last_value = NAN;
    int has_value = 0;

    while (p->cur.kind != TK_END) {
        size_t stmt_start = p->cur.pos;   // mark start BEFORE parsing
        StmtResult r = parse_statement(p);
        size_t stmt_end = p->lx.i;        // mark end AFTER parsing

        size_t len = stmt_end - stmt_start;
        char stmt[256];
        if (len >= sizeof(stmt)) len = sizeof(stmt)-1;
        memcpy(stmt, p->lx.input + stmt_start, len);
        stmt[len] = '\0';

        if (r.kind == STMT_VALUE) {
            printf("%s => %.17g\n", stmt, r.value);
            last_value = r.value;
            has_value = 1;
        } else if (r.kind == STMT_DEFINITION) {
            printf("%s => function defined\n", stmt);
        }

        accept(p, TK_SEMI);
    }

    return has_value ? last_value : 0.0;
}






/* ---------------- Demo ---------------- */
int main(void) {
const char *script =
    "f(a,b)=a*a+b*2;"
    "x=10;"
    "y=3;"
    "f(20,5);"
    "pi=3.14159;"
    "sin(pi/2);"
    "f(2,5);"
    "diff(sin(x), x);"
    "diff(sin(x), x, pi);";


    printf("Input program:\n%s\n\n", script);

    mp_parser p = {{script, 0, strlen(script)}, {0}};
    advance(&p);

    double result = parse_program(&p);

    printf("\nLast evaluated value: %.17g\n", result);

    // Optional: show defined functions and variables
    printf("\nDefined variables:\n");
    for (int i = 0; i < n_vars; i++) {
        printf("  %s = %.17g\n", vars[i].name, vars[i].value);
    }

    printf("\nDefined functions:\n");
    for (int i = 0; i < n_funcs; i++) {
        printf("  %s(", funcs[i].name);
        for (int j = 0; j < funcs[i].n_params; j++) {
            if (j > 0) printf(", ");
            printf("%s", funcs[i].params[j]);
        }
        printf(") = %s\n", funcs[i].body);
    }

    return 0;
}