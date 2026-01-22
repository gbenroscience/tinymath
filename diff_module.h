/* diff_module.h
 * Minimal symbolic differentiation for your math parser.
 * Depends on: define_func(...), lookup_func(...).
 * Compile with: -lm
 */

#ifndef DIFF_MODULE_H
#define DIFF_MODULE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* ---------- AST ---------- */

typedef enum { N_NUM, N_VAR, N_OP, N_FUNC } NodeType;

typedef struct Node {
    NodeType type;
    double num;           /* N_NUM */
    char name[32];        /* N_VAR or N_FUNC */
    char op;              /* N_OP: '+','-','*','/' */
    struct Node *a, *b;   /* binary for N_OP */
    struct Node *args[4]; /* up to 4 args for N_FUNC (sin,cos,log,exp,pow) */
    int n_args;
} Node;

static Node* nd_num(double v){ Node* n=(Node*)calloc(1,sizeof(Node)); n->type=N_NUM; n->num=v; return n; }
static Node* nd_var(const char* s){ Node* n=(Node*)calloc(1,sizeof(Node)); n->type=N_VAR; snprintf(n->name,sizeof(n->name),"%s",s); return n; }
static Node* nd_op(char op, Node* a, Node* b){ Node* n=(Node*)calloc(1,sizeof(Node)); n->type=N_OP; n->op=op; n->a=a; n->b=b; return n; }
static Node* nd_func(const char* f, Node* x){ Node* n=(Node*)calloc(1,sizeof(Node)); n->type=N_FUNC; snprintf(n->name,sizeof(n->name),"%s",f); n->args[0]=x; n->n_args=1; return n; }
static Node* nd_pow(Node* u, Node* v){ Node* n=(Node*)calloc(1,sizeof(Node)); n->type=N_FUNC; snprintf(n->name,sizeof(n->name),"%s","pow"); n->args[0]=u; n->args[1]=v; n->n_args=2; return n; }

static Node* nd_copy(Node* n){
    if(!n) return NULL;
    Node* r=(Node*)calloc(1,sizeof(Node));
    memcpy(r,n,sizeof(Node));
    if(n->type==N_OP){ r->a=nd_copy(n->a); r->b=nd_copy(n->b); }
    else if(n->type==N_FUNC){ for(int i=0;i<n->n_args;i++) r->args[i]=nd_copy(n->args[i]); }
    return r;
}

static void nd_free(Node* n){
    if(!n) return;
    if(n->type==N_OP){ nd_free(n->a); nd_free(n->b); }
    else if(n->type==N_FUNC){ for(int i=0;i<n->n_args;i++) nd_free(n->args[i]); }
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
        char id[64]; size_t j=0;
        while(lx->i<lx->len && d_is_ident_char(lx->s[lx->i]) && j<sizeof(id)-1) id[j++]=lx->s[lx->i++];
        id[j]='\0';
        if(d_accept(lx,'(')){
            Node* args[2]={0}; int n=0;
            if(!d_accept(lx,')')){
                args[n++]=d_parse_expr(lx);
                if(d_accept(lx,',')) args[n++]=d_parse_expr(lx);
                d_accept(lx,')');
            }
            if(strcmp(id,"pow")==0 && n==2) return nd_pow(args[0],args[1]);
            if(n==1) return nd_func(id,args[0]);
            return nd_var(id);
        }
        return nd_var(id);
    }
    return nd_num(0);
}

static Node* d_parse_factor(DLex* lx){
    Node* u=d_parse_primary(lx);
    if(d_accept(lx,'^')){
        Node* v=d_parse_factor(lx); /* right-assoc */
        return nd_pow(u,v);
    }
    return u;
}

static Node* d_parse_term(DLex* lx){
    Node* v=d_parse_factor(lx);
    while(1){
        if(d_accept(lx,'*')) v=nd_op('*', v, d_parse_factor(lx));
        else if(d_accept(lx,'/')) v=nd_op('/', v, d_parse_factor(lx));
        else break;
    }
    return v;
}

static Node* d_parse_expr(DLex* lx){
    Node* v=d_parse_term(lx);
    while(1){
        if(d_accept(lx,'+')) v=nd_op('+', v, d_parse_term(lx));
        else if(d_accept(lx,'-')) v=nd_op('-', v, d_parse_term(lx));
        else break;
    }
    return v;
}

static Node* parse_to_ast(const char* expr){
    DLex lx={expr,0,strlen(expr)};
    return d_parse_expr(&lx);
}

/* ---------- Differentiation ---------- */

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
            Node* u=n->args[0];
            if(strcmp(f,"sin")==0) return nd_op('*', nd_func("cos", nd_copy(u)), d_diff(u,x));
            if(strcmp(f,"cos")==0) return nd_op('*', nd_num(-1), nd_op('*', nd_func("sin", nd_copy(u)), d_diff(u,x)));
            if(strcmp(f,"exp")==0) return nd_op('*', nd_func("exp", nd_copy(u)), d_diff(u,x));
            if(strcmp(f,"log")==0) return nd_op('*', nd_op('/', nd_num(1), nd_copy(u)), d_diff(u,x));
            if(strcmp(f,"pow")==0){
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
            return nd_num(0);
        }
    }
    return nd_num(0);
}

/* ---------- Simplifier (light) ---------- */

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

/* ---------- Printer ---------- */

static void p_buf(char** out, size_t* cap, const char* s){
    size_t need=strlen(s);
    size_t cur=strlen(*out);
    if(cur+need+1 > *cap){
        *cap = (cur+need+64);
        *out = (char*)realloc(*out, *cap);
    }
    memcpy(*out+cur, s, need+1);
}

static void p_node_rec(Node* n, char** out, size_t* cap);

static void p_wrap(Node* n, char** out, size_t* cap){
    p_buf(out,cap,"("); p_node_rec(n,out,cap); p_buf(out,cap,")");
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
            p_buf(out,cap,n->name);
            break;
        case N_OP:
            if(n->op=='+'||n->op=='-'){
                p_node_rec(n->a,out,cap);
                snprintf(tmp,sizeof(tmp)," %c ", n->op); p_buf(out,cap,tmp);
                p_node_rec(n->b,out,cap);
            } else if(n->op=='*'){
                p_wrap(n->a,out,cap); p_buf(out,cap,"*"); p_wrap(n->b,out,cap);
            } else if(n->op=='/'){
                p_wrap(n->a,out,cap); p_buf(out,cap,"/"); p_wrap(n->b,out,cap);
            }
            break;
        case N_FUNC:
            if(strcmp(n->name,"pow")==0){
                p_wrap(n->args[0],out,cap); p_buf(out,cap,"^"); p_wrap(n->args[1],out,cap);
            } else {
                p_buf(out,cap,n->name); p_buf(out,cap,"(");
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

/* ---------- Public API ---------- */

/* Returns a heap-allocated string with d/d(var) expr; caller must free(). */
static char* diff_expr(const char* expr, const char* var){
    Node* ast = parse_to_ast(expr);
    Node* d   = d_diff(ast, var);
    d = s_simpl(d);
    char* s = ast_to_string(d);
    nd_free(d); nd_free(ast);
    return s;
}

/* Create a derivative function: dst_name(params...) = d/d(wrt) src_name(body) */
static int diff_func(const char* src_name, const char* wrt, const char* dst_name){
    extern struct mp_func* lookup_func(const char*);   /* provided by your parser */
    extern int define_func(const char*, char[][64], int, const char*);

    struct mp_func* f = lookup_func(src_name);
    if(!f){ fprintf(stderr,"diff_func: unknown function %s\n", src_name); return 0; }

    char* body_d = diff_expr(f->body, wrt);

    char params[16][64];
    for(int i=0;i<f->n_params;i++){
        snprintf(params[i], sizeof(params[i]), "%s", f->params[i]);
    }

    int ok = define_func(dst_name, params, f->n_params, body_d);
    free(body_d);
    return ok;
}

#endif /* DIFF_MODULE_H */
