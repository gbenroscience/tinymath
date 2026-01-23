#ifndef PARSER_API_H
#define PARSER_API_H

/* Shared function table type */
typedef struct {
    char name[64];
    char params[16][64];
    int n_params;
    char body[1024];
} mp_func;


/* Function table API exposed by the parser */
mp_func* lookup_func(const char* name);
int define_func(const char* name, char params[][64], int n_params, const char* body);

#endif /* PARSER_API_H */
