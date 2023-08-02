#ifndef LMACRO_BEGUN

#include <boost/preprocessor/cat.hpp>

#pragma push_macro("LMACRO_BEGUN")
#define LMACRO_BEGUN
#pragma push_macro("L")
#pragma push_macro("L2")
#pragma push_macro("L0")
#pragma push_macro("F")

#define L0(Expr, ...) [__VA_ARGS__] -> decltype(Expr) { return Expr; }
#define L(Expr, ...)                                                                               \
    [__VA_ARGS__]<class BOOST_PP_CAT(T, __LINE__)>([[maybe_unused]] BOOST_PP_CAT(T, __LINE__) &&   \
                                                   x) -> decltype((Expr))                          \
        requires(requires { Expr; })                                                               \
    {                                                                                              \
        return Expr;                                                                               \
    }
#define L2(Expr, ...)                                                                              \
    [__VA_ARGS__]<class BOOST_PP_CAT(T, __LINE__), class BOOST_PP_CAT(U, __LINE__)>(               \
        [[maybe_unused]] BOOST_PP_CAT(T, __LINE__) && x,                                           \
        [[maybe_unused]] BOOST_PP_CAT(U, __LINE__) && y) -> decltype((Expr))                       \
        requires(requires { Expr; })                                                               \
    {                                                                                              \
        return Expr;                                                                               \
    }
#define M0(Expr, ...) [__VA_ARGS__] mutable -> decltype(auto) { return Expr; }
#define M(Expr, ...)                                                                               \
    [__VA_ARGS__]<class BOOST_PP_CAT(T, __LINE__)>(                                                \
        [[maybe_unused]] const BOOST_PP_CAT(T, __LINE__) & x) mutable -> decltype(auto) {          \
        return Expr;                                                                               \
    }
#define M2(Expr, ...)                                                                              \
    [__VA_ARGS__]<class BOOST_PP_CAT(T, __LINE__), class BOOST_PP_CAT(U, __LINE__)>(               \
        [[maybe_unused]] const BOOST_PP_CAT(T, __LINE__) & x,                                      \
        [[maybe_unused]] const BOOST_PP_CAT(U, __LINE__) & y) mutable -> decltype(auto) {          \
        return Expr;                                                                               \
    }
#define F(F, ...)                                                                                  \
    [__VA_ARGS__]<class... Args>(Args && ...args) noexcept(                                        \
        noexcept(F(static_cast<Args &&>(args)...))) -> decltype(F(static_cast<Args &&>(args)...))  \
        requires requires {                                                                        \
            F(static_cast<Args &&>(args))                                                          \
            ...;                                                                                   \
        }                                                                                          \
    {                                                                                              \
        return F(static_cast<Args &&>(args)...);                                                   \
    }
#else
#pragma pop_macro("LMACRO_BEGUN")
#pragma pop_macro("L")
#pragma pop_macro("L2")
#pragma pop_macro("L0")
#pragma pop_macro("M")
#pragma pop_macro("M2")
#pragma pop_macro("F")
#endif
