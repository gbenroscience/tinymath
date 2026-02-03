/*
 * test_runner.c
 *
 * Simple C test runner for the tinymath executable.
 *
 * Usage:
 *   Compile:
 *     gcc -O2 -std=c99 -Wall -o test_runner test_runner.c
 *
 *   Run:
 *     ./test_runner /path/to/tinymath
 *     (on Windows: test_runner.exe tinymath.exe)
 *
 * The runner uses the convention that tinymath accepts "@filename" as its
 * first argument (the patched main() described earlier). For each test it
 * writes a temporary file containing the script and invokes:
 *    tinymath "@tmpfile"
 *
 * It parses numeric outputs of the form:
 *    <stmt> => <number>
 * and compares them against expected numbers, within a small tolerance.
 *
 * It also supports substring checks for symbolic outputs.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <io.h>
#define popen _popen
#define pclose _pclose
#define unlink _unlink
#else
#include <unistd.h>
#endif

typedef struct {
    const char *name;
    const char *script;
    double expected_numbers[8];
    int expected_numbers_count;
    const char *expected_substrings[8];
    int expected_substrings_count;
} TestCase;

/* Helper: write script to temporary file (simple naming test_<i>.tmp) */
static int write_tmp_script(const char *prefix, int idx, const char *script, char *out_path, size_t out_path_len)
{
    int n = snprintf(out_path, out_path_len, "%s_test_%d.tmp", prefix, idx);
    if (n < 0 || (size_t)n >= out_path_len) return 0;
    FILE *f = fopen(out_path, "wb");
    if (!f) return 0;
    fwrite(script, 1, strlen(script), f);
    fclose(f);
    return 1;
}

/* Helper: run command with popen and capture stdout into a dynamically allocated buffer */
static char *run_and_capture(const char *cmd)
{
    FILE *fp = popen(cmd, "r");
    if (!fp) return NULL;

    size_t cap = 4096;
    size_t len = 0;
    char *buf = malloc(cap);
    if (!buf) { pclose(fp); return NULL; }
    buf[0] = '\0';

    char line[1024];
    while (fgets(line, sizeof(line), fp))
    {
        size_t l = strlen(line);
        if (len + l + 1 > cap)
        {
            cap = (len + l + 1) * 2;
            char *tmp = realloc(buf, cap);
            if (!tmp) { free(buf); pclose(fp); return NULL; }
            buf = tmp;
        }
        memcpy(buf + len, line, l);
        len += l;
        buf[len] = '\0';
    }

    pclose(fp);
    return buf;
}

/* Extract numbers from output by looking for "=> <number>" patterns in order */
static int extract_numbers(const char *out, double *nums, int max_nums)
{
    const char *p = out;
    int count = 0;
    while (p && *p && count < max_nums)
    {
        const char *arrow = strstr(p, "=>");
        if (!arrow) break;
        arrow += 2;
        /* skip whitespace */
        while (*arrow && (*arrow == ' ' || *arrow == '\t')) ++arrow;
        if (!*arrow) break;
        char *endptr;
        double v = strtod(arrow, &endptr);
        if (endptr == arrow) {
            /* no number parsed; try to skip and continue */
            p = arrow;
        } else {
            nums[count++] = v;
            p = endptr;
        }
    }
    return count;
}

static int approx_equal(double a, double b, double tol)
{
    double diff = a - b;
    if (diff < 0) diff = -diff;
    return diff <= tol;
}

int main(int argc, char **argv)
{
    if (argc < 2)
    {
        fprintf(stderr, "Usage: %s /path/to/tinymath\n", argv[0]);
        return 2;
    }
    const char *tinymath_path = argv[1];

    /* Basic tests embedded here */
         TestCase tests[] = {
        {
            "basic_math",
            "1+2; 3*4; 2^3; sin(pi/2);",
            {3.0, 12.0, 8.0, 1.0}, 4,
            {NULL}, 0
        },
        {
            "user_func_eval",
            "g(x)=x^3-2*x+1; g(3);",
            {22.0}, 1,
            {NULL}, 0
        },
        {
            "diff_eval",
            "g(x)=x^3-2*x+1; diff(g(x), x, 3);",
            {25.0}, 1,
            {NULL}, 0
        },
        {
            "stats",
            "sum(1,2,3,4); mean(1,2,3,4);",
            {10.0, 2.5}, 2,
            {NULL}, 0
        },
        {
            "diff_symbolic",
            "g(x)=x^3-2*x+1; diff(g(x), x);",
            {0}, 0,
            {"^(2)", "3"}, 2
        },
        {
            "diff_check",
            "h(x)=cos(3*x); diff(h(x), x, 3);",
            {0}, 0,
            {"^(2)", "3"}, 2
        }
        
    };
    int n_tests = sizeof(tests) / sizeof(tests[0]);

    int passed = 0;

    /* temp file prefix (use program name without path if available) */
    const char *prefix = "t";
    {
        const char *s = strrchr(tinymath_path, '/');
        if (!s) s = strrchr(tinymath_path, '\\');
        if (s) prefix = s + 1;
        else prefix = tinymath_path;
    }

    for (int i = 0; i < n_tests; ++i)
    {
        char tmpname[256];
        if (!write_tmp_script(prefix, i, tests[i].script, tmpname, sizeof(tmpname)))
        {
            fprintf(stderr, "[%s] failed to create temp script file\n", tests[i].name);
            continue;
        }

        /* build command: tinymath "@tmpname" */
        char cmd[1024];
#ifdef _WIN32
        /* On Windows ensure program path is quoted if contains spaces */
        snprintf(cmd, sizeof(cmd), "\"%s\" @%s", tinymath_path, tmpname);
#else
        snprintf(cmd, sizeof(cmd), "%s @%s", tinymath_path, tmpname);
#endif

        char *out = run_and_capture(cmd);
        if (!out)
        {
            fprintf(stderr, "[%s] failed to run command: %s\n", tests[i].name, cmd);
            unlink(tmpname);
            continue;
        }

        /* Extract numbers */
        double found[16];
        int found_n = extract_numbers(out, found, 16);

        int ok = 1;
        /* check expected numbers */
        if (tests[i].expected_numbers_count > 0)
        {
            if (found_n < tests[i].expected_numbers_count)
            {
                fprintf(stderr, "[%s] expected %d numeric results but found %d\n", tests[i].name,
                        tests[i].expected_numbers_count, found_n);
                ok = 0;
            }
            else
            {
                for (int k = 0; k < tests[i].expected_numbers_count; ++k)
                {
                    double expv = tests[i].expected_numbers[k];
                    double gotv = found[k];
                    if (!approx_equal(expv, gotv, 1e-6)) {
                        fprintf(stderr, "[%s] numeric mismatch idx %d: expected %.12g got %.12g\n",
                                tests[i].name, k, expv, gotv);
                        ok = 0;
                    }
                }
            }
        }

        /* check expected substrings */
        for (int s = 0; s < tests[i].expected_substrings_count; ++s)
        {
            const char *sub = tests[i].expected_substrings[s];
            if (sub && strstr(out, sub) == NULL)
            {
                fprintf(stderr, "[%s] expected substring not found: '%s'\n", tests[i].name, sub);
                ok = 0;
            }
        }

        if (ok)
        {
            printf("[OK] %s\n", tests[i].name);
            passed++;
        }
        else
        {
            printf("[FAIL] %s\n--- output ---\n%s\n--- end ---\n", tests[i].name, out);
        }

        free(out);
        unlink(tmpname);
    }

    printf("Passed %d/%d tests\n", passed, n_tests);
    return (passed == n_tests) ? 0 : 1;
}