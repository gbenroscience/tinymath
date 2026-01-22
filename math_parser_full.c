/*
 * math_parser_full.c
 * Recursive-descent math parser with variables, built-in functions,
 * and user-defined functions (definitions + calls).
 *
 * Build: cc -O2 -std=c99 -lm math_parser_full.c -o math_parser_full
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* ---------------- Symbol Table ---------------- */

typedef struct {
    char name[64];
    double value;
} mp_var;

#define MAX_VARS 256
static mp_var vars[MAX_VARS];
static int n_vars = 0;

static int set_var(const char *name, double val) {
    for (int i = 0; i < n_vars; i++) {
        if (strcmp(vars[i].name, name) == 0) {
            vars[i].value = val;
            return 1;
        }
    }
    if (n_vars < MAX_VARS) {
        strncpy(vars[n_vars].name, name, sizeof(vars[n_vars].name)-1);
        vars[n_vars].name[sizeof(vars[n_vars].name)-1] = '\0';
        vars[n_vars].value = val;
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

/* ---------------- Function Table (user-defined) ---------------- */

typedef struct {
    char name[64];
    char params[16][64];  /* up to 16 parameters */
    int n_params;
    char body[1024];      /* expression string for body */
} mp_func;

#define MAX_FUNCS 128
static mp_func funcs[MAX_FUNCS];
static int n_funcs = 0;

static int define_func(const char *name, char params[][64], int n_params, const char *body) {
    if (n_funcs >= MAX_FUNCS) {
        fprintf(stderr, "Function table full\n");
        return 0;
    }
    strncpy(funcs[n_funcs].name, name, sizeof(funcs[n_funcs].name)-1);
    funcs[n_funcs].name[sizeof(funcs[n_funcs].name)-1] = '\0';
    funcs[n_funcs].n_params = n_params;
    for (int i = 0; i < n_params; i++) {
        strncpy(funcs[n_funcs].params[i], params[i], sizeof(funcs[n_funcs].params[i])-1);
        funcs[n_funcs].params[i][sizeof(funcs[n_funcs].params[i])-1] = '\0';
    }
    strncpy(funcs[n_funcs].body, body, sizeof(funcs[n_funcs].body)-1);
    funcs[n_funcs].body[sizeof(funcs[n_funcs].body)-1] = '\0';
    n_funcs++;
    return 1;
}

static mp_func* lookup_func(const char *name) {
    for (int i = 0; i < n_funcs; i++) {
        if (strcmp(funcs[i].name, name) == 0) return &funcs[i];
    }
    return NULL;
}

/* ---------------- Lexer ---------------- */

typedef enum {
    TK_END=0, TK_NUM, TK_IDENT,
    TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH, TK_CARET,
    TK_LPAREN, TK_RPAREN, TK_COMMA, TK_EQ, TK_SEMI
} mp_tok_kind;

typedef struct {
    mp_tok_kind kind;
    double num;
    char ident[64];
    size_t pos;  /* start position in input */
} mp_token;

typedef struct {
    const char *input;
    size_t i, len;
} mp_lexer;

static void skip_ws(mp_lexer *lx) {
    while (lx->i < lx->len && isspace((unsigned char)lx->input[lx->i])) lx->i++;
}

static mp_token next_token(mp_lexer *lx) {
    skip_ws(lx);
    mp_token t = {0};
    t.pos = lx->i;
    if (lx->i >= lx->len) { t.kind = TK_END; return t; }
    char c = lx->input[lx->i];

    /* number: digits[.digits][exp] */
    if (isdigit((unsigned char)c) || (c=='.' && lx->i+1<lx->len && isdigit((unsigned char)lx->input[lx->i+1]))) {
        char buf[128]; size_t j=0;
        /* integer/decimal part */
        while (lx->i<lx->len && (isdigit((unsigned char)lx->input[lx->i]) || lx->input[lx->i]=='.')) {
            if (j<sizeof(buf)-1) buf[j++]=lx->input[lx->i];
            lx->i++;
        }
        /* exponent part */
        if (lx->i<lx->len && (lx->input[lx->i]=='e' || lx->input[lx->i]=='E')) {
            if (j<sizeof(buf)-1) buf[j++]=lx->input[lx->i];
            lx->i++;
            if (lx->i<lx->len && (lx->input[lx->i]=='+' || lx->input[lx->i]=='-')) {
                if (j<sizeof(buf)-1) buf[j++]=lx->input[lx->i];
                lx->i++;
            }
            while (lx->i<lx->len && isdigit((unsigned char)lx->input[lx->i])) {
                if (j<sizeof(buf)-1) buf[j++]=lx->input[lx->i];
                lx->i++;
            }
        }
        buf[j]='\0';
        t.kind=TK_NUM; t.num=strtod(buf,NULL);
        return t;
    }

    /* identifier */
    if (isalpha((unsigned char)c) || c=='_') {
        char buf[64]; size_t j=0;
        while (lx->i<lx->len && (isalnum((unsigned char)lx->input[lx->i]) || lx->input[lx->i]=='_')) {
            if (j<sizeof(buf)-1) buf[j++]=lx->input[lx->i];
            lx->i++;
        }
        buf[j]='\0';
        t.kind=TK_IDENT; strncpy(t.ident,buf,sizeof(t.ident)-1); t.ident[sizeof(t.ident)-1]='\0';
        return t;
    }

    lx->i++;
    switch(c){
        case '+': t.kind=TK_PLUS; break;
        case '-': t.kind=TK_MINUS; break;
        case '*': t.kind=TK_STAR; break;
        case '/': t.kind=TK_SLASH; break;
        case '^': t.kind=TK_CARET; break;
        case '(': t.kind=TK_LPAREN; break;
        case ')': t.kind=TK_RPAREN; break;
        case ',': t.kind=TK_COMMA; break;
        case '=': t.kind=TK_EQ; break;
        case ';': t.kind=TK_SEMI; break;
        default:  t.kind=TK_END;  break;
    }
    return t;
}

/* ---------------- Parser ---------------- */

typedef struct {
    mp_lexer lx;
    mp_token cur;
} mp_parser;

static void advance(mp_parser *p){ p->cur=next_token(&p->lx); }
static int accept(mp_parser *p, mp_tok_kind k){ if(p->cur.kind==k){ advance(p); return 1;} return 0; }

static double parse_expr(mp_parser *p); /* forward */

/* Built-in functions */
static double call_builtin(const char *name, double *args, int n) {
    if(strcmp(name,"sin")==0 && n==1) return sin(args[0]);
    if(strcmp(name,"cos")==0 && n==1) return cos(args[0]);
    if(strcmp(name,"tan")==0 && n==1) return tan(args[0]);
    if(strcmp(name,"sqrt")==0 && n==1) return sqrt(args[0]);
    if(strcmp(name,"log")==0 && n==1) return log(args[0]);
    if(strcmp(name,"exp")==0 && n==1) return exp(args[0]);
    if(strcmp(name,"abs")==0 && n==1) return fabs(args[0]);
    if(strcmp(name,"pow")==0 && n==2) return pow(args[0],args[1]);
    fprintf(stderr,"Unknown or wrong-arity built-in function: %s\n",name);
    return NAN;
}

/* Evaluate a user-defined function body with a new scope */
static double call_user_func(mp_func *f, double *args, int n) {
    if(n!=f->n_params){
        fprintf(stderr,"Wrong arity for %s: expected %d, got %d\n",f->name,f->n_params,n);
        return NAN;
    }
    /* Save old values and bind parameters */
    double oldvals[16]; int had_old[16];
    for(int i=0;i<n;i++){
        had_old[i]=lookup_var(f->params[i],&oldvals[i]);
        set_var(f->params[i],args[i]);
    }

    /* Evaluate body */
    mp_parser sub={{f->body,0,strlen(f->body)}, {0}};
    advance(&sub);
    double result=parse_expr(&sub);

    /* Restore old values */
    for(int i=0;i<n;i++){
        if(had_old[i]) set_var(f->params[i],oldvals[i]);
    }
    return result;
}

static double parse_primary(mp_parser *p){
    if(p->cur.kind==TK_NUM){ double v=p->cur.num; advance(p); return v; }

    if(p->cur.kind==TK_IDENT){
        char name[64]; strncpy(name,p->cur.ident,sizeof(name)-1); name[sizeof(name)-1]='\0';
        advance(p);

        /* Function call? */
        if(accept(p,TK_LPAREN)){
            double args[32]; int n=0;
            if(!accept(p,TK_RPAREN)){
                do { args[n++]=parse_expr(p); } while(accept(p,TK_COMMA));
                if(!accept(p,TK_RPAREN)) fprintf(stderr,"Expected ')' after arguments\n");
            }
            mp_func *uf=lookup_func(name);
            if(uf) return call_user_func(uf,args,n);
            return call_builtin(name,args,n);
        }

        /* Variable */
        double v;
        if(lookup_var(name,&v)) return v;
        fprintf(stderr,"Unknown variable: %s\n",name);
        return NAN;
    }

    if(accept(p,TK_LPAREN)){ double v=parse_expr(p); if(!accept(p,TK_RPAREN)) fprintf(stderr,"Expected ')'\n"); return v; }
    if(accept(p,TK_MINUS)) return -parse_primary(p);
    if(accept(p,TK_PLUS))  return  parse_primary(p);

    fprintf(stderr,"Unexpected token near position %zu\n", p->cur.pos);
    return NAN;
}

static double parse_factor(mp_parser *p){
    /* Right-associative exponentiation */
    double v=parse_primary(p);
    if(accept(p,TK_CARET)){ double rhs=parse_factor(p); v=pow(v,rhs); }
    return v;
}

static double parse_term(mp_parser *p){
    double v=parse_factor(p);
    while(p->cur.kind==TK_STAR||p->cur.kind==TK_SLASH){
        mp_tok_kind op=p->cur.kind; advance(p);
        double rhs=parse_factor(p);
        if(op==TK_STAR) v*=rhs; else v/=rhs;
    }
    return v;
}

static double parse_expr(mp_parser *p){
    double v=parse_term(p);
    while(p->cur.kind==TK_PLUS||p->cur.kind==TK_MINUS){
        mp_tok_kind op=p->cur.kind; advance(p);
        double rhs=parse_term(p);
        if(op==TK_PLUS) v+=rhs; else v-=rhs;
    }
    return v;
}

/* Parse a function definition: name '(' params ')' '=' body ';' */
static int parse_func_def(mp_parser *p, const char *fname_first) {
    /* We enter with the function name already consumed in parse_statement.
       Next token must be '(' */
    if(!accept(p,TK_LPAREN)) return 0;

    char params[16][64]; int n=0;
    if(p->cur.kind==TK_IDENT){
        do {
            strncpy(params[n], p->cur.ident, sizeof(params[n])-1);
            params[n][sizeof(params[n])-1]='\0';
            n++;
            advance(p);
        } while(accept(p,TK_COMMA));
    }
    if(!accept(p,TK_RPAREN)) { fprintf(stderr,"Expected ')' in function definition\n"); return 0; }
    if(!accept(p,TK_EQ))     { fprintf(stderr,"Expected '=' in function definition\n"); return 0; }

    /* Capture body text until ';' (raw substring) */
    size_t body_start = p->lx.i; /* current lexer index points to start of next token */
    /* Advance tokens until ';' or end, but we need the raw slice. */
    while(p->cur.kind!=TK_SEMI && p->cur.kind!=TK_END) advance(p);
    size_t body_end = p->lx.i;
    size_t len = (body_end>body_start)? (body_end - body_start) : 0;
    char body[1024];
    if(len >= sizeof(body)) len = sizeof(body)-1;
    memcpy(body, p->lx.input + body_start, len);
    body[len] = '\0';

    if(p->cur.kind==TK_SEMI) advance(p); /* consume ';' */

    char fname[64]; strncpy(fname, fname_first, sizeof(fname)-1); fname[sizeof(fname)-1]='\0';
    if(!define_func(fname, params, n, body)) return 0;
    return 1;
}

/* Statement:
   - Function definition: IDENT '(' params ')' '=' expr ';'
   - Assignment: IDENT '=' expr
   - Expression: expr
*/
static double parse_statement(mp_parser *p){
    if(p->cur.kind==TK_IDENT){
        char name[64]; strncpy(name,p->cur.ident,sizeof(name)-1); name[sizeof(name)-1]='\0';
        advance(p);

        /* Lookahead: function definition starts with '(' after name */
        if(p->cur.kind==TK_LPAREN){
            /* We’ll parse and register the function, return NAN for the statement value */
            if(!parse_func_def(p, name)) fprintf(stderr,"Invalid function definition for %s\n", name);
            return NAN;
        }

        /* Assignment: name '=' expr */
        if(accept(p,TK_EQ)){
            double val=parse_expr(p);
            set_var(name,val);
            return val;
        }

        /* Otherwise, treat as variable usage at statement start */
        double v;
        if(lookup_var(name,&v)) return v;
        fprintf(stderr,"Unknown variable: %s\n",name);
        return NAN;
    }

    /* Plain expression statement */
    return parse_expr(p);
}

/* Program: sequence of statements separated by ';' (semicolon optional after last) */
static double parse_program(mp_parser *p){
    double result=NAN;
    while(p->cur.kind!=TK_END){
        result=parse_statement(p);
        /* optional semicolon between statements */
        accept(p,TK_SEMI);
    }
    return result;
}

/* ---------------- Demo ---------------- */

int main(void){
    /* Demo script:
       - Define f(a,b,c)
       - Assign x,y,z
       - Evaluate f(x,y,z)
       - Then a mixed expression using variables and built-ins
    */
    const char *script =
        "f(a,b,c)=sin(a+b)-cos(a-b)+3*c^2;"
        "x=2; y=3; z=4;"
        "f(x,y,z);"
        "u=pow(2,3); v=log(8); 3*sin(4+log(8))^(sin(2))-88+23/45;";

    mp_parser p={{script,0,strlen(script)}, {0}};
    advance(&p);
    double res=parse_program(&p);

    /* Show some values */
    double fx=0;
    mp_func *f = lookup_func("f");
    if (f) {
        double args[3];
        double x,y,z;
        lookup_var("x",&x); lookup_var("y",&y); lookup_var("z",&z);
        args[0]=x; args[1]=y; args[2]=z;
        fx = call_user_func(f,args,3);
    }

    double u=0,v=0;
    lookup_var("u",&u);
    lookup_var("v",&v);

    printf("f(x,y,z) = %.6f\n", fx);
    printf("u = %.6f, v = %.6f\n", u, v);
    printf("Final result of last statement = %.6f\n", res);
    return 0;
}
