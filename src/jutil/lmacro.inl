#ifndef LMACRO_BEGUN
#define LMACRO_BEGUN

#include "macro.h"

#pragma push_macro("L")
#pragma push_macro("L2")
#pragma push_macro("L0")
#pragma push_macro("F")

#define L0(Expr, ...) [__VA_ARGS__] -> decltype(Expr) { return Expr; }
#define L(Expr, ...)                                                                               \
    [__VA_ARGS__]<class CAT(T, __LINE__)>([[maybe_unused]] CAT(T, __LINE__) &&                     \
                                          x) -> decltype((Expr))                                   \
        requires(requires { Expr; })                                                               \
    {                                                                                              \
        return Expr;                                                                               \
    }
#define L2(Expr, ...)                                                                              \
    [__VA_ARGS__]<class CAT(T, __LINE__), class CAT(U, __LINE__)>(                                 \
        [[maybe_unused]] CAT(T, __LINE__) && x,                                                    \
        [[maybe_unused]] CAT(U, __LINE__) && y) -> decltype((Expr))                                \
        requires(requires { Expr; })                                                               \
    {                                                                                              \
        return Expr;                                                                               \
    }
#define M0(Expr, ...) [__VA_ARGS__] mutable -> decltype(auto) { return Expr; }
#define M(Expr, ...)                                                                               \
    [__VA_ARGS__]<class CAT(T, __LINE__)>([[maybe_unused]] const CAT(T, __LINE__) &                \
                                          x) mutable -> decltype(auto) { return Expr; }
#define M2(Expr, ...)                                                                              \
    [__VA_ARGS__]<class CAT(T, __LINE__), class CAT(U, __LINE__)>(                                 \
        [[maybe_unused]] const CAT(T, __LINE__) & x,                                               \
        [[maybe_unused]] const CAT(U, __LINE__) & y) mutable -> decltype(auto) { return Expr; }
#define F(F, ...)                                                                                  \
    []<class... CAT(Ts, __LINE__)>(CAT(Ts, __LINE__) && ...CAT(xs, __LINE__)) noexcept(noexcept(   \
        F(__VA_ARGS__ __VA_OPT__(, ) static_cast<CAT(Ts, __LINE__) &&>(CAT(xs, __LINE__))...)))    \
        -> decltype(F(                                                                             \
            __VA_ARGS__ __VA_OPT__(, ) static_cast<CAT(Ts, __LINE__) &&>(CAT(xs, __LINE__))...))   \
        requires(requires {                                                                        \
            F(__VA_ARGS__ __VA_OPT__(, ) static_cast<CAT(Ts, __LINE__) &&>(CAT(xs, __LINE__))...); \
        })                                                                                         \
    {                                                                                              \
        return F(                                                                                  \
            __VA_ARGS__ __VA_OPT__(, ) static_cast<CAT(Ts, __LINE__) &&>(CAT(xs, __LINE__))...);   \
    }
#else
#undef LMACRO_BEGUN
#pragma pop_macro("L")
#pragma pop_macro("L2")
#pragma pop_macro("L0")
#pragma pop_macro("M")
#pragma pop_macro("M2")
#pragma pop_macro("F")
#endif
