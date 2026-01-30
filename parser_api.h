#ifndef PARSER_API_H
#define PARSER_API_H

#include <stddef.h> /* for size_t */

#define MAX_FUNC_PARAMS 32
#define MAX_IDENT_LEN 64

/* API version. Bump when making incompatible changes. */
#define PARSER_API_VERSION 2

/* Opaque forward declarations so the public mp_func can hold pointers
   to internal AST/arena structures without exposing their definition. */
typedef struct ASTNode ASTNode;
typedef struct ArenaBlock ArenaBlock;
typedef struct Arena Arena;

/* Forward-declare mp_context so the API can accept a context pointer.
   The concrete mp_context is defined by the implementation but we expose
   the type name here so callers can hold pointers to it. */
typedef struct mp_context mp_context;
typedef struct parser_t parser_t;

/* Public mp_func: includes public metadata and optional implementation-owned
   pointers to a parsed AST and arena. Implementations that pre-parse a
   function body may populate root/arena; older implementations may leave
   them NULL. */
typedef struct {
    char name[MAX_IDENT_LEN];
    char params[MAX_FUNC_PARAMS][MAX_IDENT_LEN];
    int n_params;
    char *body;        /* heap-allocated null-terminated string (owned by the function table) */
    ASTNode *root;     /* optional: opaque pointer to parsed AST root (implementation-owned) */
    Arena *arena;      /* optional: opaque per-function arena (implementation-owned) */
} mp_func;

/* Lookup a user-defined function by name (returns NULL if not found).
 * Requires an explicit mp_context* to avoid hidden global/TLS usage.
 */
mp_func* lookup_func(mp_context *ctx, const char* name);

/* Define a function by name, copying the parameter names and the body slice.
 * The implementation takes ownership of the copied body string.
 *
 * Signature:
 *  - ctx: context to store function into
 *  - name: function name
 *  - params: array of parameter name strings
 *  - n_params: number of parameters
 *  - body_start/body_len: slice of source text containing the body (not null-terminated)
 */
int define_func(mp_context *ctx, const char *name, const char params[][MAX_IDENT_LEN], int n_params,
                const char* body_start, size_t body_len);

/* Context management and execution helpers
 * - ctx_create: allocate and initialize a new context (caller must call ctx_destroy).
 * - ctx_destroy: free a context and all associated resources.
 * - exec_with_ctx: execute a script using an existing context (does not free the context).
 *
 * Note: If you intend to share a context across threads, you must provide your own
 * synchronization (this library does not add internal locking).
 */
mp_context* ctx_create(void);
void ctx_destroy(mp_context *ctx);
double exec_with_ctx(mp_context *ctx, const char *script);

#endif /* PARSER_API_H */