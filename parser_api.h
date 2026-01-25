#ifndef PARSER_API_H
#define PARSER_API_H

#include <stddef.h> /* for size_t */

#define MAX_FUNC_PARAMS 32
#define MAX_IDENT_LEN 64

/* mp_func: function metadata stored by the parser/library.
 * - name, params are fixed-size buffers
 * - body is heap-allocated by define_func (library takes ownership)
 */
typedef struct {
    char name[MAX_IDENT_LEN];
    char params[MAX_FUNC_PARAMS][MAX_IDENT_LEN];
    int n_params;
    char *body;           /* heap-allocated null-terminated string (owned by the function table) */
} mp_func;

/* Lookup a user-defined function by name (returns NULL if not found). */
mp_func* lookup_func(const char* name);

/* Define a function by copying the parameter names and the body slice.
 * The implementation should copy the body (body_start, body_len) and store
 * a null-terminated heap-allocated string in mp_func->body.
 */
int define_func(const char* name, const char params[][MAX_IDENT_LEN], int n_params,
                const char* body_start, size_t body_len);
                /*
                mp_func* lookup_func(const char* name);
int define_func(const char* name, char params[][64], int n_params,
                const char* body_start, size_t body_len);
                */

#endif /* PARSER_API_H */