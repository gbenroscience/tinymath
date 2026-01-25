#ifndef PARSER_API_H
#define PARSER_API_H

#define MAX_FUNC_PARAMS 32
#define MAX_IDENT_LEN 64
typedef struct {
    char name[64];
    char params[32][64];  // increased
    int n_params;
    char *body;           // dynamic
} mp_func;

mp_func* lookup_func(const char* name);
int define_func(const char* name, char params[][64], int n_params,
                const char* body_start, size_t body_len);

#endif /* PARSER_API_H */