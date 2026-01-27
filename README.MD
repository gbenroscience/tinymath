# tinymath

What v4.1 gives you

    No hidden TLS globals: parser state (vars, funcs, trig/symbolic mode, flags) lives in mp_context and is passed explicitly via mp_parser->ctx.
    ctx_create / ctx_destroy / exec_with_ctx let callers create long-lived contexts on the heap and reuse them.
    Two threads that each create and use their own mp_context (the recommended usage) can run concurrently with no races in the library itself.

What is still NOT thread-safe

    Sharing a single mp_context* concurrently from multiple threads is not safe. The library does not perform any internal synchronization. Concurrent callers can:
        Race when mutating ctx->vars or ctx->funcs (realloc/resizing),
        See inconsistent lookups (if another thread is mid-mutation),
        Crash if one thread calls ctx_destroy while another is using the context.
    Reentrancy caveat: exec_with_ctx uses the passed context directly. If you call exec_with_ctx(ctx, ...) from one thread while another thread is also using ctx, you need synchronization.
    Error paths/memory leaks: some early returns can leak temporary allocations (not a direct race but causes resource problems in long-running concurrent programs).
    If you spawn threads inside a script and pass pointers into those threads to parser structures (function bodies, etc.), lifetimes must be carefully managed by your code.
    Platform notes: printing (printf) is typically thread-safe in libc, but side-effects like changing global locale or non-thread-safe library calls could cause issues.
