## tinymath

**tinymath** is a lean, fast and efficient math parser written in native C for maximum speed.
It has, inbuilt into it, the ability to do expression parsing, variables arithmetic, trigonometric computations, functions creation and evaluation, derivatives evaluation and computation and statistics.

### Expression parsing
```
2+2;
4+3*2-5^(3+sin(7));
```

### Variables
```
a11=5.2;b13=3.8;
sin(a11-4*b13);
```

### Functions
```
f(x,y)=3*x^3-5*x^2-4.2*x-13;
g(x)=sin(x+12);
```

### Calculus
```
diff(3*x^2,x);
diff(3*x^2,x,3);
```
The first. evaluates the derivative with respect to $$x$$ and the second computes the value of the derivative at $$x=3$$

### Statistics
A number of statistic functions are supported including:
```
min, max, sum, prod, mean|avg, rng, mrng, std_err, var, std, svar, sstd, median|med, mode, rms 
```

These all take comma separated lists of numbers|variables|functions_to_be_evaluated|or_whatever_resolves_to_a_number as argument.
For examples:
```
a=5;b=3.141;
median(sin(4),8, -19, 24*cos(a-2*b),..)
```

## Run zah codez:
To run the code, store your math expression(s) in a const char* e.g:
```C
const char *script = "32*sin(3^cos(2));";
    double result = exec(script);
```
You may enter as many expressions as you wish as long as you separate each one by a semi-colon

For instance:
```C
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
        "28*ln(32)+sum(3,5,7,9,sin(pi/2),12);"
        "MODE();"
        "f(2,5);"
        "4%3;"
        "10%3;"
        "(13%9)+2^(5%3);"
        "-10%3;"
        "GRAD();"
        "sin(pi/2);"
        "a11=5.2;b13=3.8;"
        "sin(a11-4*b13);"
        "g(x)=x^3-2*x+1;"
        "g(3);"
        "diff(g(x)-sin(x), x);"
        "diff(g(x),x, 3);"
        "diff(sin(x^3-3*x-2), x);"
        "diff(diff(sin(x^3), x), x);"
        "diff(diff(diff(sin(x), x), x), x);"
        "diff(sin(x), x, pi);";

    printf("Input program:\n%s\n\n", script);
    double result = exec(script);
    printf("\nLast evaluated value: %.17g\n", result);
    return 0;
}
```

## Thread safety
There are no hidden TLS globals: parser state (vars, funcs, trig/symbolic mode, flags) lives in `mp_context` and is passed explicitly via `mp_parser->ctx`.

`ctx_create` / `ctx_destroy` / `exec_with_ctx` let callers create long-lived contexts on the heap and reuse them.

Two threads that each create and use their own `mp_context` (the recommended usage) can run concurrently with no races in the library itself.

## Safe caller-side locking patterns and 2 self-contained example programs

### Example A — Per-thread contexts (no locking)
Each thread creates its own context, uses it, and destroys it when done.
No shared library state is modified concurrently; this is the simplest, fastest approach.



```C
// per_thread_example.c
// Each thread uses its own ctx (no locking required).
#include <stdio.h>
#include <pthread.h>
#include "parser_api.h"   // v4.1 header
// tinymath.c compiled and linked provides exec_with_ctx, ctx_create, ctx_destroy

void *worker(void *arg)
{
    const char *script = (const char *)arg;
    mp_context *ctx = ctx_create();
    if (!ctx) return NULL;

    double r = exec_with_ctx(ctx, script);
    printf("Thread %lu did script -> last value = %.17g\n", (unsigned long)pthread_self(), r);

    ctx_destroy(ctx);
    return NULL;
}

int main(void)
{
    pthread_t t1, t2;
    const char *s1 = "x=10; y=2; x*y;";
    const char *s2 = "f(a)=a^2; f(5);";

    pthread_create(&t1, NULL, worker, (void*)s1);
    pthread_create(&t2, NULL, worker, (void*)s2);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);
    return 0;
}
```

```
Compile with: gcc -O2 -std=c99 -Wall -pthread -o per_thread_example per_thread_example.c
```



### Example B — Shared context with caller-side mutex (coarse-grained locking)

A single mp_context is shared among threads.
Caller uses a mutex to serialize all calls that touch that context.
This is straightforward and safe, but serializes executions (no parallelism on that context).

```C
// shared_ctx_example.c
// All access to shared_ctx is serialized using a pthread mutex.
#include <stdio.h>
#include <pthread.h>
#include "parser_api.h"

static mp_context *shared_ctx;
static pthread_mutex_t ctx_mutex = PTHREAD_MUTEX_INITIALIZER;

void *workerA(void *arg)
{
    const char *script = (const char *)arg;

    // Lock around the entire exec_with_ctx call
    pthread_mutex_lock(&ctx_mutex);
    double r = exec_with_ctx(shared_ctx, script);
    pthread_mutex_unlock(&ctx_mutex);

    printf("[A] script result: %.17g\n", r);
    return NULL;
}

void *workerB(void *arg)
{
    const char *script = (const char *)arg;

    // Another thread uses the same shared_ctx; must lock
    pthread_mutex_lock(&ctx_mutex);
    // Example: define a function in shared context
    exec_with_ctx(shared_ctx, script);
    pthread_mutex_unlock(&ctx_mutex);

    printf("[B] executed script: %s\n", script);
    return NULL;
}

int main(void)
{
    pthread_t t1, t2;

    // create shared context once
    shared_ctx = ctx_create();
    if (!shared_ctx) return 1;

    const char *s1 = "x=100; y=3; x+y;";
    const char *s2 = "f(a)=a*2;";

    pthread_create(&t1, NULL, workerA, (void*)s1);
    pthread_create(&t2, NULL, workerB, (void*)s2);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    // safe to destroy only after all threads have finished using it
    ctx_destroy(shared_ctx);
    return 0;
}
```

```
Compile with: gcc -O2 -std=c99 -Wall -pthread -o shared_ctx_example shared_ctx_example.c
```


## Tests:
To run the provided tests, on Windows, fire up a command prompt window, navigate to the project directory and type **run_tests.bat** and then, press **Enter**.

On Linux systems, fire up a terminal window, navigate to the project directory and type **run_tests.sh** and then, press **Enter**