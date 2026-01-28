#ifndef DIFF_MODULE_H
#define DIFF_MODULE_H

/* diff_module.h
 *
 * Symbolic differentiation helpers.
 * - Adds support for expanding user-defined functions (via mp_context*)
 *   before differentiating, so expressions like diff(g(x), x) work.
 *
 * Intended to be included into tinymath.c (single TU). It uses the
 * parser_api.h functions lookup_func(ctx, ...) and define_func(ctx, ...).
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "parser_api.h"

/* ---------- AST ---------- */

typedef enum { N_NUM, N_VAR, N_OP, N_FUNC } NodeType;

typedef struct Node {
    NodeType type;
    double num;           /* N_NUM */
    char *name;           /* N_VAR or N_FUNC (heap-allocated; may be NULL) */
    char op;              /* N_OP: '+','-','*','/' */
    struct Node *a, *b;   /* binary for N_OP */
    struct Node **args;   /* heap-allocated array of Node* for N_FUNC */
    int n_args;
} Node;

/* Node constructors / helpers (allocate and initialize) */

static Node* nd_alloc(void) {
    Node *n = (Node*)calloc(1, sizeof(Node));
    return n;
}

static Node* nd_num(double v) {
    Node* n = nd_alloc();
    n->type = N_NUM;
    n->num = v;
    return n;
}

static Node* nd_var(const char* s) {
    Node* n = nd_alloc();
    n->type = N_VAR;
    n->name = strdup(s ? s : "");
    return n;
}

static Node* nd_op(char op, Node* a, Node* b) {
    Node* n = nd_alloc();
    n->type = N_OP;
    n->op = op;
    n->a = a;
    n->b = b;
    return n;
}

/* Create a function node with given name and argument list */
static Node* nd_func_n(const char* f, Node** argv, int n_args) {
    Node* n = nd_alloc();
    n->type = N_FUNC;
    n->name = strdup(f ? f : "");
    if (n_args > 0) {
        n->args = (Node**)malloc(sizeof(Node*) * n_args);
        for (int i = 0; i < n_args; ++i) n->args[i] = argv[i];
    } else {
        n->args = NULL;
    }
    n->n_args = n_args;
    return n;
}

/* Convenience single-arg and two-arg factories */
static Node* nd_func(const char* f, Node* x) {
    Node* argv[1] = { x };
    return nd_func_n(f, argv, 1);
}

static Node* nd_pow(Node* u, Node* v) {
    Node* argv[2] = { u, v };
    return nd_func_n("pow", argv, 2);
}

/* Deep copy and free helpers */

static Node* nd_copy(Node* n) {
    if (!n) return NULL;
    Node* r = nd_alloc();
    r->type = n->type;
    r->num = n->num;
    r->op = n->op;
    r->a = NULL; r->b = NULL;
    r->name = n->name ? strdup(n->name) : NULL;
    r->n_args = n->n_args;
    r->args = NULL;
    if (n->type == N_OP) {
        r->a = nd_copy(n->a);
        r->b = nd_copy(n->b);
    } else if (n->type == N_FUNC) {
        if (n->n_args > 0) {
            r->args = (Node**)malloc(sizeof(Node*) * n->n_args);
            for (int i = 0; i < n->n_args; ++i)
                r->args[i] = nd_copy(n->args[i]);
        }
    }
    return r;
}

static void nd_free(Node* n) {
    if (!n) return;
    if (n->type == N_OP) {
        nd_free(n->a);
        nd_free(n->b);
    } else if (n->type == N_FUNC) {
        for (int i = 0; i < n->n_args; ++i) nd_free(n->args[i]);
        free(n->args);
    }
    free(n->name);
    free(n);
}

/* ---------- Tiny parser to AST (expr subset) ---------- */

typedef struct { const char* s; size_t i, len; } DLex;

static void d_skip(DLex* lx){ while(lx->i<lx->len && isspace((unsigned char)lx->s[lx->i])) lx->i++; }
static int d_peek(DLex* lx){ d_skip(lx); return (lx->i<lx->len)? lx->s[lx->i] : 0; }
static int d_accept(DLex* lx, char c){ d_skip(lx); if(d_peek(lx)==c){ lx->i++; return 1; } return 0; }

static Node* d_parse_expr(DLex* lx); /* fwd */

static Node* d_parse_number(DLex* lx){
    d_skip(lx);
    size_t start=lx->i;
    int seen_digit=0;
    while(lx->i<lx->len){
        char c=lx->s[lx->i];
        if(isdigit((unsigned char)c)){ seen_digit=1; lx->i++; }
        else if(c=='.' || c=='e' || c=='E' || c=='+' || c=='-'){ lx->i++; }
        else break;
    }
    if(!seen_digit) return NULL;
    char buf[128]; size_t n=lx->i-start; if(n>=sizeof(buf)) n=sizeof(buf)-1;
    memcpy(buf,lx->s+start,n); buf[n]='\0';
    return nd_num(strtod(buf,NULL));
}

static int d_is_ident_char(int c){ return isalnum((unsigned char)c) || c=='_'; }

static Node* d_parse_primary(DLex* lx){
    d_skip(lx);
    if(d_accept(lx,'(')){ Node* e=d_parse_expr(lx); d_accept(lx,')'); return e; }
    if(d_accept(lx,'+')) return d_parse_primary(lx);
    if(d_accept(lx,'-')) return nd_op('-', nd_num(0), d_parse_primary(lx));

    Node* num=d_parse_number(lx);
    if(num) return num;

    if(lx->i<lx->len && (isalpha((unsigned char)lx->s[lx->i]) || lx->s[lx->i]=='_')){
        char id[MAX_IDENT_LEN]; size_t j=0;
        while(lx->i<lx->len && d_is_ident_char(lx->s[lx->i]) && j<sizeof(id)-1) id[j++]=lx->s[lx->i++];
        id[j]='\0';
        if(d_accept(lx,'(')){
            /* parse comma-separated args into a small dynamic vector */
            Node **args = NULL;
            int n = 0;
            if(!d_accept(lx,')')){
                do {
                    Node *arg = d_parse_expr(lx);
                    Node **tmp = realloc(args, sizeof(Node*) * (n + 1));
                    if (!tmp) { /* allocation failure - cleanup and abort */ 
                        for (int k = 0; k < n; ++k) nd_free(args[k]);
                        free(args);
                        return NULL;
                    }
                    args = tmp;
                    args[n++] = arg;
                } while (d_accept(lx,','));
                d_accept(lx,')');
            }
            if(strcmp(id,"pow")==0 && n==2) {
                Node* r = nd_pow(args[0], args[1]);
                free(args);
                return r;
            }
            if(n==1) {
                Node* r = nd_func(id, args[0]);
                free(args);
                return r;
            }
            if (n > 1) {
                Node *r = nd_func_n(id, args, n);
                free(args);
                return r;
            }
            return nd_var(id);
        }
        return nd_var(id);
    }
    return nd_num(0);
}

static Node* d_parse_factor(DLex* lx){
    Node* u=d_parse_primary(lx);
    if(d_accept(lx,'^')){
        Node* v=d_parse_factor(lx);
        return nd_pow(u,v);
    }
    return u;
}

static Node* d_parse_term(DLex* lx){
    Node* n=d_parse_factor(lx);
    while(1){
        if(d_accept(lx,'*')) n=nd_op('*', n, d_parse_factor(lx));
        else if(d_accept(lx,'/')) n=nd_op('/', n, d_parse_factor(lx));
        else break;
    }
    return n;
}

static Node* d_parse_expr(DLex* lx){
    Node *n=d_parse_term(lx);
    while(1){
        if(d_accept(lx,'+')) n=nd_op('+', n, d_parse_term(lx));
        else if(d_accept(lx,'-')) n=nd_op('-', n, d_parse_term(lx));
        else break;
    }
    return n;
}

/* ---------- Helpers for expanding user functions ---------- */

/* forward declarations so functions can be used before their definitions */
static Node* substitute_vars(Node* n, const char* param, Node* repl);
static Node* expand_node(Node* n, mp_context* ctx);

/* substitute_vars: walk AST n, replace any N_VAR equal to 'param' with a deep copy of repl.
   Returns (possibly new) Node* for the subtree.
*/
static Node* substitute_vars(Node* n, const char* param, Node* repl) {
    if (!n) return NULL;
    if (n->type == N_VAR) {
        if (n->name && strcmp(n->name, param) == 0) {
            nd_free(n);
            return nd_copy(repl);
        }
        return n;
    } else if (n->type == N_OP) {
        n->a = substitute_vars(n->a, param, repl);
        n->b = substitute_vars(n->b, param, repl);
        return n;
    } else if (n->type == N_FUNC) {
        for (int i = 0; i < n->n_args; ++i)
            n->args[i] = substitute_vars(n->args[i], param, repl);
        return n;
    }
    return n;
}

/* expand_node: recursively expand user-defined function calls using ctx.
   If a function call name matches a user-defined function, parse its body,
   substitute actual args for parameters, expand recursively and return the
   expanded subtree (freeing the original node).
*/
static Node* expand_node(Node* n, mp_context* ctx) {
    if (!n) return NULL;
    if (n->type == N_OP) {
        n->a = expand_node(n->a, ctx);
        n->b = expand_node(n->b, ctx);
        return n;
    } else if (n->type == N_FUNC) {
        /* first expand args */
        for (int i = 0; i < n->n_args; ++i) n->args[i] = expand_node(n->args[i], ctx);

        /* check for user-defined function */
        mp_func* f = lookup_func(ctx, n->name);
        if (!f) return n; /* leave as-is (could be builtin or unknown) */

        /* parse the function body into AST */
        DLex lx = { f->body, 0, strlen(f->body) };
        Node* body_ast = d_parse_expr(&lx);
        if (!body_ast) return n;

        /* substitute parameters with provided args */
        for (int i = 0; i < f->n_params; ++i) {
            Node* replacement = (i < n->n_args) ? n->args[i] : nd_num(0);
            /* For safety, use nd_copy in substitution (substitute_vars copies) */
            body_ast = substitute_vars(body_ast, f->params[i], replacement);
        }

        /* recursively expand the newly created body (to handle nested user calls) */
        Node* expanded = expand_node(body_ast, ctx);

        /* free the original call node and its args (they were consumed/copied) */
        for (int i = 0; i < n->n_args; ++i) nd_free(n->args[i]);
        free(n->args);
        free(n->name);
        free(n);

        return expanded;
    }
    /* N_NUM or N_VAR */
    return n;
}

/* ---------- Differentiator and simplifier (uses Node API) ---------- */

static int is_var(Node* n, const char* x){ return n->type==N_VAR && strcmp(n->name,x)==0; }
static int is_num(Node* n, double v){ return n->type==N_NUM && fabs(n->num - v) < 1e-15; }

static Node* d_diff(Node* n, const char* x){
    if(!n) return nd_num(0);
    switch(n->type){
        case N_NUM: return nd_num(0);
        case N_VAR: return nd_num(is_var(n,x)? 1 : 0);
        case N_OP: {
            Node *u=n->a, *v=n->b;
            switch(n->op){
                case '+': return nd_op('+', d_diff(u,x), d_diff(v,x));
                case '-': return nd_op('-', d_diff(u,x), d_diff(v,x));
                case '*': return nd_op('+',
                                  nd_op('*', d_diff(u,x), nd_copy(v)),
                                  nd_op('*', nd_copy(u), d_diff(v,x)));
                case '/': return nd_op('/',
                                  nd_op('-', nd_op('*', d_diff(u,x), nd_copy(v)),
                                               nd_op('*', nd_copy(u), d_diff(v,x))),
                                  nd_pow(nd_copy(v), nd_num(2)));
                default: return nd_num(0);
            }
        }
        case N_FUNC: {
            const char* f=n->name;
            /* handle built-in single-arg functions */
            if (n->n_args <= 0) return nd_num(0);
            Node* u = n->args[0];
            if(strcmp(f,"sin")==0) return nd_op('*', nd_func("cos", nd_copy(u)), d_diff(u,x));
            if(strcmp(f,"cos")==0) return nd_op('*', nd_num(-1), nd_op('*', nd_func("sin", nd_copy(u)), d_diff(u,x)));
            if(strcmp(f,"exp")==0) return nd_op('*', nd_func("exp", nd_copy(u)), d_diff(u,x));
            if(strcmp(f,"log")==0) return nd_op('*', nd_op('/', nd_num(1), nd_copy(u)), d_diff(u,x));
            if(strcmp(f,"pow")==0 && n->n_args==2){
                Node* a=n->args[0]; Node* b=n->args[1];
                if(b->type==N_NUM){
                    double k=b->num;
                    return nd_op('*',
                           nd_num(k),
                           nd_op('*', nd_pow(nd_copy(a), nd_num(k-1)), d_diff(a,x)));
                } else {
                    /* d/dx a^b = a^b * ( b' * ln(a) + b * a'/a ) */
                    return nd_op('*',
                           nd_pow(nd_copy(a), nd_copy(b)),
                           nd_op('+',
                               nd_op('*', d_diff(b,x), nd_func("log", nd_copy(a))),
                               nd_op('*', nd_copy(b),
                                     nd_op('/', d_diff(a,x), nd_copy(a)))
                           ));
                }
            }
            /* Unknown function (shouldn't happen after expansion) */
            return nd_num(0);
        }
    }
    return nd_num(0);
}

/* Simplifier (light) */

static Node* s_simpl(Node* n){
    if(!n) return n;
    if(n->type==N_OP){
        n->a=s_simpl(n->a); n->b=s_simpl(n->b);
        Node *u=n->a, *v=n->b;
        if(n->op=='+'){
            if(is_num(u,0)) return v;
            if(is_num(v,0)) return u;
        } else if(n->op=='-'){
            if(is_num(v,0)) return u;
        } else if(n->op=='*'){
            if(is_num(u,0) || is_num(v,0)) return nd_num(0);
            if(is_num(u,1)) return v;
            if(is_num(v,1)) return u;
        } else if(n->op=='/'){
            if(is_num(u,0)) return nd_num(0);
            if(is_num(v,1)) return u;
        }
    } else if(n->type==N_FUNC){
        for(int i=0;i<n->n_args;i++) n->args[i]=s_simpl(n->args[i]);
    }
    return n;
}

/* Pretty-print AST -> string */

static void p_buf(char** out, size_t* cap, const char* s){
    size_t need=strlen(s);
    size_t cur=strlen(*out);
    if(cur+need+1 >= *cap){
        *cap = (cur+need+64);
        *out = (char*)realloc(*out, *cap);
    }
    memcpy(*out+cur, s, need+1);
}
static void p_node_rec(Node* n, char** out, size_t* cap){
    char tmp[64];
    if(!n){ p_buf(out,cap,"0"); return; }
    switch(n->type){
        case N_NUM:
            snprintf(tmp,sizeof(tmp),"%.15g", n->num);
            p_buf(out,cap,tmp);
            break;
        case N_VAR:
            p_buf(out,cap,n->name ? n->name : "");
            break;
        case N_OP:
            if(n->op=='+'||n->op=='-'){
                p_node_rec(n->a,out,cap);
                snprintf(tmp,sizeof(tmp)," %c ", n->op); p_buf(out,cap,tmp);
                p_node_rec(n->b,out,cap);
            } else if(n->op=='*'){
                p_buf(out,cap,"("); p_node_rec(n->a,out,cap); p_buf(out,cap,")");
                p_buf(out,cap,"*");
                p_buf(out,cap,"("); p_node_rec(n->b,out,cap); p_buf(out,cap,")");
            } else if(n->op=='/'){
                p_buf(out,cap,"("); p_node_rec(n->a,out,cap); p_buf(out,cap,")");
                p_buf(out,cap,"/");
                p_buf(out,cap,"("); p_node_rec(n->b,out,cap); p_buf(out,cap,")");
            }
            break;
        case N_FUNC:
            if(n->name && strcmp(n->name,"pow")==0 && n->n_args==2){
                p_buf(out,cap,"("); p_node_rec(n->args[0],out,cap); p_buf(out,cap,")^("); p_node_rec(n->args[1],out,cap); p_buf(out,cap,")");
            } else {
                p_buf(out,cap,n->name ? n->name : "");
                p_buf(out,cap,"(");
                for(int i=0;i<n->n_args;i++){
                    if(i) p_buf(out,cap,",");
                    p_node_rec(n->args[i],out,cap);
                }
                p_buf(out,cap,")");
            }
            break;
    }
}

static char* ast_to_string(Node* n){
    char* out=(char*)calloc(1,256);
    size_t cap=256; out[0]='\0';
    p_node_rec(n,&out,&cap);
    return out;
}

/* ---------- Public API for diff module ---------- */

/* diff_expr:
 * - Parses expr, expands any user-defined functions (from ctx) by inlining their
 *   bodies with parameter substitution, differentiates with respect to var,
 *   simplifies and returns heap-allocated string. Caller must free().
 */
static char* diff_expr_ctx(mp_context* ctx, const char* expr, const char* var) {
    if (!expr || !var) {
        fprintf(stderr, "diff_expr_ctx: null input\n");
        return strdup("NaN");
    }

    DLex lx = { expr, 0, strlen(expr) };
    Node* ast = d_parse_expr(&lx);
    if (!ast) {
        fprintf(stderr, "diff_expr_ctx: failed to parse expression: %s\n", expr);
        return strdup("NaN");
    }

    /* Expand user-defined functions using ctx */
    Node* expanded = expand_node(ast, ctx);
    if (!expanded) {
        nd_free(ast);
        fprintf(stderr, "diff_expr_ctx: expansion failed\n");
        return strdup("NaN");
    }

    Node* d = d_diff(expanded, var);
    if (!d) {
        fprintf(stderr, "diff_expr_ctx: differentiation failed\n");
        nd_free(expanded);
        return strdup("NaN");
    }

    Node* simplified = s_simpl(d);
    char* result = ast_to_string(simplified);

    nd_free(simplified);
    nd_free(expanded);

    return result;
}


/* Backwards-compatible wrapper: diff_expr (no ctx) behaves as before (no user-function expansion).
 * Keep for code that expects diff_expr(expr,var). */
static char* diff_expr(const char* expr, const char* var) {
    if (!expr || !var) {
        fprintf(stderr, "diff_expr: null input\n");
        return strdup("NaN");
    }

    DLex lx = { expr, 0, strlen(expr) };
    Node* ast = d_parse_expr(&lx);
    if (!ast) {
        fprintf(stderr, "diff_expr: failed to parse expression: %s\n", expr);
        return strdup("NaN");
    }

    Node* d = d_diff(ast, var);
    if (!d) {
        fprintf(stderr, "diff_expr: differentiation failed\n");
        nd_free(ast);
        return strdup("NaN");
    }

    Node* simplified = s_simpl(d);
    char* result = ast_to_string(simplified);

    nd_free(simplified);
    nd_free(ast);

    return result;
}
/* Create a derivative function (same as before) */
static int diff_func(mp_context* ctx, const char* src_name, const char* wrt, const char* dst_name){
    if (!ctx) { fprintf(stderr, "diff_func: null context\n"); return 0; }

    mp_func* f = lookup_func(ctx, src_name);
    if(!f){ fprintf(stderr,"diff_func: unknown function %s\n", src_name); return 0; }

    char* body_d = diff_expr_ctx(ctx, f->body, wrt);
    if (!body_d) return 0;

    char params[MAX_FUNC_PARAMS][MAX_IDENT_LEN];
    for(int i=0;i<f->n_params;i++){
        snprintf(params[i], MAX_IDENT_LEN, "%s", f->params[i]);
    }

    int ok = define_func(ctx, dst_name, params, f->n_params, body_d, strlen(body_d));
    free(body_d);
    return ok;
}

#endif /* DIFF_MODULE_H */