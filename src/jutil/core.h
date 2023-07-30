#pragma once

#include <array>
#include <bit>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/comma_if.hpp>
#include <boost/preprocessor/comparison/equal.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/identity.hpp>
#include <boost/preprocessor/punctuation/remove_parens.hpp>
#include <boost/preprocessor/repetition/repeat.hpp>
#include <boost/preprocessor/seq/fold_left.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/stringize.hpp>
#include <boost/preprocessor/variadic/to_seq.hpp>
#include <ranges>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <type_traits>
#include <utility>

#include "lmacro.inl"

namespace jutil
{
#ifdef __cpp_lib_hardware_interference_size
using std::hardware_constructive_interference_size;
using std::hardware_destructive_interference_size;
#else
constexpr std::size_t hardware_constructive_interference_size = 64;
constexpr std::size_t hardware_destructive_interference_size  = 64;
#endif

template <class T, template <class...> class Tmpl>
concept instance_of = requires(std::remove_cvref_t<T> &x) { []<class... Ts>(Tmpl<Ts...> &) {}(x); };

template <class T>
concept constant = requires(std::remove_cvref_t<T> &x) {
    []<class U, U Val>(std::integral_constant<U, Val> &) {}(x);
};

template <class From, class To>
concept static_castable = requires(From &&x) { static_cast<To>(static_cast<From &&>(x)); };
template <class From, class To>
concept bit_castable = sizeof(To) == sizeof(From) and std::is_trivially_copyable_v<To> and
                       std::is_trivially_copyable_v<From>;
template <class From, class To>
concept opaque_castable = static_castable<From, To> || bit_castable<From, To>;
template <class To, class From>
constexpr To opaque_cast(From &&x) noexcept
{
    if constexpr (static_castable<From, To>)
        return static_cast<To>(static_cast<From &&>(x));
    else
        return std::bit_cast<To>(static_cast<From &&>(x));
}
template <class To, opaque_castable<To> From>
constexpr To opaque_cast(From &x) noexcept
{
    if constexpr (static_castable<From, To>)
        return static_cast<To>(x);
    else
        return std::bit_cast<To>(x);
}
template <class T>
using opaque = std::aligned_storage_t<sizeof(T), alignof(T)>;

template <class T, class... Us>
concept one_of = (std::same_as<T, Us> or ...);

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 5104)
#endif
#define JUTIL_wstr_exp(X) L#X
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#define WSTRINGIFY(X) JUTIL_wstr_exp(X)

#ifndef NDEBUG
#define JUTIL_UNREACHABLE()                                                                        \
    do {                                                                                           \
        __builtin_trap();                                                                          \
        exit(1);                                                                                   \
    } while (0)
#else
#define JUTIL_UNREACHABLE()                                                                        \
    do {                                                                                           \
        [[assume(0)]];                                                                             \
        std::unreachable();                                                                        \
    } while (0)
#endif
#define JUTIL_NO_DEFAULT()                                                                         \
    default: JUTIL_UNREACHABLE()
#ifdef _MSC_VER
#define JUTIL_INLINE            inline __forceinline
#define JUTIL_NOINLINE          __declspec(noinline)
#define JUTIL_NO_UNIQUE_ADDRESS [[no_unique_address]] [[msvc::no_unique_address]]
#define JUTIL_TRAP()            __debugbreak()
#else
#define JUTIL_INLINE            inline __attribute__((always_inline))
#define JUTIL_NOINLINE          __attribute__((noinline))
#define JUTIL_NO_UNIQUE_ADDRESS [[no_unique_address]]
#if defined(__GNUC__) || defined(__GNUG__)
#define JUTIL_HOSTED              _GLIBCXX_HOSTED
#define JUTIL_PACKED              __attribute__((packed))
#define JUTIL_PUSH_DIAG(X)        _Pragma("GCC diagnostic push") X
#define JUTIL_POP_DIAG()          _Pragma("GCC diagnostic pop")
#define JUTIL_WNO_UNUSED_VALUE    _Pragma("GCC diagnostic ignored \"-Wunused-value\"")
#define JUTIL_WNO_UNUSED_PARAM    _Pragma("GCC diagnostic ignored \"-Wunused-parameter\"")
#define JUTIL_WNO_UNUSED_VARIABLE _Pragma("GCC diagnostic ignored \"-Wunused-variable\"")
#define JUTIL_WNO_SHADOW          _Pragma("GCC diagnostic ignored \"-Wshadow\"")
#define JUTIL_WNO_PARENTHESES     _Pragma("GCC diagnostic ignored \"-Wparentheses\"")
#define JUTIL_WNO_SEQUENCE        _Pragma("GCC diagnostic ignored \"-Wsequence-point\"")
#define JUTIL_WNO_CCAST           _Pragma("GCC diagnostic ignored \"-Wold-style-cast\"")
#define JUTIL_WNO_SUBOBJ_LINKAGE  _Pragma("GCC diagnostic ignored \"-Wsubobject-linkage\"")
#define JUTIL_WNO_EMPTY_BODY      _Pragma("GCC diagnostic ignored \"-Wempty-body\"")
#define JUTIL_WNO_DANGLING_ELSE   _Pragma("GCC diagnostic ignored \"-Wdangling-else\"")
#define JUTIL_WNO_PEDANTIC        _Pragma("GCC diagnostic ignored \"-Wpedantic\"")
#else
#error "unsupported compiler"
#endif
#define JUTIL_TRAP() __builtin_trap()
#endif
#define JUTIL_CI constexpr JUTIL_INLINE

template <bool F, class T, class U>
JUTIL_CI std::conditional_t<F, T, U> &&if_(T &&t, U &&u) noexcept
{
    if constexpr (F) {
        return static_cast<T &&>(t);
    } else {
        return static_cast<U &&>(u);
    }
}

#if JUTIL_DEBUG
#define JUTIL_dbge(...) __VA_ARGS__
#else
#define JUTIL_dbge(...) (void)0
#endif

#ifndef NDEBUG
#define DBGEXPR(...)  __VA_ARGS__
#define DBGSTMNT(...) __VA_ARGS__
#define JUTIL_c_u_impl(E, U, For, ...)                                                             \
    ([&]<class BOOST_PP_CAT(JaT, __LINE__)>(BOOST_PP_CAT(JaT, __LINE__) &&                         \
                                            BOOST_PP_CAT(jae, __LINE__))                           \
         -> decltype(BOOST_PP_CAT(jae, __LINE__),                                                  \
                     std::type_identity<BOOST_PP_CAT(JaT, __LINE__)>{})::type {                    \
        if (!(BOOST_PP_CAT(jae, __LINE__) For)) {                                                  \
            if constexpr (jutil::opaque_castable<uintmax_t, BOOST_PP_CAT(JaT, __LINE__)>)          \
                fprintf(                                                                           \
                    stderr,                                                                        \
                    "\033[2m" __FILE__ ":" BOOST_PP_STRINGIZE(__LINE__) ":\033[0m assertion failed: \033[1m%s \033[2m(%#jx)\033[0;1m "   \
                                  "%s\033[0m" BOOST_PP_IF(U, ": %s \033[2m(%#x)\033[0m", "") "\n", \
                        (__VA_OPT__((void)) #E __VA_OPT__(, __VA_ARGS__)),                         \
                        jutil::opaque_cast<uintmax_t>(BOOST_PP_CAT(jae, __LINE__)),                \
                        #For BOOST_PP_COMMA_IF(U) BOOST_PP_REMOVE_PARENS(                          \
                            BOOST_PP_IIF(U, (strerror(errno), errno), ())));                       \
            else                                                                                   \
                fprintf(stderr, "\033[2m" __FILE__ ":" BOOST_PP_STRINGIZE(__LINE__) ":\033[0m assertion failed: \033[1m%s %s\033[0m\n",  \
                                    (__VA_OPT__((void)) #E __VA_OPT__(, __VA_ARGS__)), #For);      \
            JUTIL_TRAP();                                                                          \
        }                                                                                          \
        return BOOST_PP_CAT(jae, __LINE__);                                                        \
    })
#define JUTIL_c_impl(E, For, ...)                                                                  \
    (jutil::if_<std::string_view{#For} == ".U">(                                                   \
        JUTIL_c_u_impl(E, 1, != -1 __VA_OPT__(, __VA_ARGS__)),                                     \
        JUTIL_c_u_impl(E, 0, For __VA_OPT__(, __VA_ARGS__))))(E)
#else
#define DBGEXPR(...) ((void)0)
#define DBGSTMNT(...)
#define JUTIL_c_impl(E, ...)                                                                       \
    [&]<class BOOST_PP_CAT(JaT, __LINE__)>(                                                        \
        BOOST_PP_CAT(JaT, __LINE__) && BOOST_PP_CAT(jae, __LINE__)) -> BOOST_PP_CAT(JaT,           \
                                                                                    __LINE__) {    \
        return BOOST_PP_CAT(jae, __LINE__);                                                        \
    }(E)
#endif

#define JUTIL_FAIL(Msg)                                                                            \
    ([]<bool BOOST_PP_CAT(F, __LINE__) = false>() {                                                \
        static_assert(BOOST_PP_CAT(F, __LINE__), Msg);                                             \
    })()
#define JUTIL_c_exp_fold_op(d, acc, x) JUTIL_c_impl(acc, x, BOOST_PP_CAT(msg, __LINE__))
#define JUTIL_c_exp_fold(E, For0, ...)                                                             \
    ([&]<class BOOST_PP_CAT(JaeT, __LINE__)>(                                                      \
         BOOST_PP_CAT(JaeT, __LINE__) && BOOST_PP_CAT(e, __LINE__)) -> BOOST_PP_CAT(JaeT,          \
                                                                                    __LINE__) {    \
        JUTIL_PUSH_DIAG(JUTIL_WNO_SHADOW JUTIL_WNO_UNUSED_VARIABLE);                               \
        static constexpr auto BOOST_PP_CAT(msg, __LINE__) = #E;                                    \
        return BOOST_PP_SEQ_FOLD_LEFT(                                                             \
            JUTIL_c_exp_fold_op,                                                                   \
            JUTIL_c_impl(static_cast<BOOST_PP_CAT(JaeT, __LINE__) &&>(BOOST_PP_CAT(e, __LINE__)),  \
                         For0, BOOST_PP_CAT(msg, __LINE__)),                                       \
            BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__));                                                \
        JUTIL_POP_DIAG()                                                                           \
    })(E)
#define JUTIL_c_exp(E, For0, ...)                                                                  \
    BOOST_PP_IF(BOOST_PP_CHECK_EMPTY(__VA_ARGS__), JUTIL_c_impl, JUTIL_c_exp_fold)                 \
    (E, For0, __VA_ARGS__)
#define CHECK(E, ...)                                                                              \
    BOOST_PP_IF(BOOST_PP_CHECK_EMPTY(__VA_ARGS__), JUTIL_c_impl, JUTIL_c_exp)(E, __VA_ARGS__)
#define CHECKZ(E) CHECK(E, == 0)
#define CHECKU(E) CHECK(E, .U)

//
// callable traits (function call expression only)
//
// clang-format off
template <class T, class... Args>
concept callable = requires(T f) {
    f(std::declval<Args>()...);
};
template <class T, class R, class... Args>
concept callable_r = requires(T f) {
    { f(std::declval<Args>()...) } -> std::same_as<R>;
};
template <class T, class... Args>
concept nothrow_callable = requires(T f) {
    { f(std::declval<Args>()...) } noexcept;
};
template <class T, class R, class... Args>
concept nothrow_callable_r = requires(T f) {
    { f(std::declval<Args>()...) } noexcept -> std::same_as<R>;
};
// clang-format on
template <class T, class... Args>
concept predicate = callable_r<T, bool, Args...>;
template <class T, class... Args>
constexpr inline bool is_callable = callable<T, Args...>;
template <class R, class T, class... Args>
constexpr inline bool is_callable_r = callable_r<T, R, Args...>;
template <class T, class... Args>
constexpr inline bool is_nothrow_callable = nothrow_callable<T, Args...>;
template <class R, class T, class... Args>
constexpr inline bool is_nothrow_callable_r = nothrow_callable_r<T, R, Args...>;
template <class T, class... Args>
using call_result = decltype(std::declval<T>()(std::declval<Args>()...));

//
// allocator
//
#if JUTIL_HOSTED
template <class Al, class T>
concept allocator = //
    requires { std::allocator_traits<Al>{}; } &&
    std::same_as<typename std::allocator_traits<Al>::value_type, T>;
#endif

//
// make_{signed,unsigned}
//
template <class T>
[[nodiscard]] JUTIL_CI std::make_signed_t<T> to_signed(T x) noexcept
{
    return static_cast<std::make_signed_t<T>>(x);
}
template <class T>
[[nodiscard]] JUTIL_CI std::make_unsigned_t<T> to_unsigned(T x) noexcept
{
    return static_cast<std::make_unsigned_t<T>>(x);
}

//
// sex
//

template <std::integral T, std::integral U>
[[nodiscard]] JUTIL_CI T sex(const U x) noexcept
{
    return static_cast<T>(static_cast<std::make_signed_t<T>>( //
        static_cast<std::make_signed_t<U>>(x)));
}

//
// to uint/ptr
//
[[nodiscard]] JUTIL_CI uintptr_t to_uint(void *x) noexcept { return std::bit_cast<uintptr_t>(x); }
[[nodiscard]] JUTIL_CI void *to_ptr(uintptr_t x) noexcept { return std::bit_cast<void *>(x); }

//
// Duff's device
//
template <std::size_t Factor = 16>
JUTIL_CI void duffs(std::integral auto n, callable auto &&f) noexcept(noexcept(f()))
{
    if (!n) return;
#define JUTIL_dd_c(z, j, fact)                                                                     \
    case (j ? fact - j : 0):                                                                       \
        f();                                                                                       \
        BOOST_PP_IF(BOOST_PP_EQUAL(j, BOOST_PP_DEC(fact)), break, [[fallthrough]]);
    if constexpr (Factor == 1) {
        while (n--)
            f();
    }
#define JUTIL_dd_fact(z, _, fact)                                                                  \
    else if constexpr (Factor == fact)                                                             \
    {                                                                                              \
        switch (auto i = (n + fact - 1) / fact; n % fact) {                                        \
            do {                                                                                   \
                BOOST_PP_REPEAT(fact, JUTIL_dd_c, fact)                                            \
                JUTIL_NO_DEFAULT();                                                                \
            } while (--i);                                                                         \
        }                                                                                          \
    }
    BOOST_PP_SEQ_FOR_EACH(JUTIL_dd_fact, ~, (2)(4)(8)(16)(32)(64))
    else JUTIL_FAIL("Factor must be one of: 1,2,4,8,16,32,64");
#undef JUTIL_dd_fact
#undef JUTIL_dd_c
}

//
// never
//
struct never {
    [[noreturn]] JUTIL_CI void operator()(auto &&...) const noexcept { JUTIL_UNREACHABLE(); };
};

//
// drop
//
template <std::size_t N, class T, std::size_t M>
[[nodiscard]] JUTIL_CI std::array<T, M - N> drop(const std::array<T, M> &arr) noexcept
{
    std::array<T, M - N> res{};
    for (auto i = 0uz, j = N; i < res.size();)
        res[i++] = arr[j++];
    return res;
}

//
// take
//
template <std::ptrdiff_t N, class T, std::size_t M>
[[nodiscard]] JUTIL_CI std::array<T, (N < 0 ? M + N : N)> take(const std::array<T, M> &arr) noexcept
{
    std::array<T, (N < 0 ? M + N : N)> res{};
    for (auto i = 0uz; i < res.size(); ++i)
        res[i] = arr[i];
    return res;
}

//
// overload
//
template <class... Fs>
struct overload : Fs... {
    using Fs::operator()...;
};
template <class... Fs>
overload(Fs &&...) -> overload<Fs...>;

//
// fn_it
//
template <class F>
struct fn_it {
    F f_;
    using difference_type = std::ptrdiff_t;
    struct proxy {
        fn_it &it;
        template <class T>
        JUTIL_CI auto operator=(T &&x) const noexcept(noexcept(it.f_(static_cast<T &&>(x))))
            -> decltype(it.f_(static_cast<T &&>(x)))
        {
            return it.f_(static_cast<T &&>(x));
        }
    };
    JUTIL_CI proxy operator*() noexcept { return {*this}; }
    JUTIL_CI bool operator==(const fn_it &it) const noexcept { return std::addressof(it) == this; }
    JUTIL_CI bool operator!=(const fn_it &it) const noexcept { return !(*this == it); }
    JUTIL_CI fn_it &operator++() noexcept { return *this; }
    JUTIL_CI fn_it operator++(int) noexcept { return {static_cast<F &&>(f_)}; }
};
template <class F>
fn_it(F &&) -> fn_it<F>;

//
// getter
//
template <std::size_t I>
struct getter {
    template <class T>
    [[nodiscard]] JUTIL_CI auto operator()(T &&x) const
        noexcept(noexcept(std::get<I>(static_cast<T &&>(x))))
            -> decltype(std::get<I>(static_cast<T &&>(x)))
    {
        return std::get<I>(static_cast<T &&>(x));
    }
};
constexpr inline getter<0> fst{};
constexpr inline getter<1> snd{};

//
// is
//
template <class T, auto... Is>
using iseq = std::integer_sequence<T, Is...>;
template <auto... Xs>
using iseqty = iseq<decltype((Xs + ...)), Xs...>;
template <auto... Xs>
constexpr inline iseqty<Xs...> idxseq{};
template <std::size_t I>
using idxty = std::integral_constant<std::size_t, I>;

//
// forward
//
template <class... Ts>
JUTIL_CI std::tuple<Ts &&...>
forward(Ts &&...xs) noexcept(std::is_nothrow_constructible_v<std::tuple<Ts &&...>, Ts &&...>)
{
    return {static_cast<Ts &&>(xs)...};
}

//
// caster
//
template <class T>
struct caster {
    template <class U>
    [[nodiscard]] JUTIL_CI T operator()(U &&x) const
        noexcept(noexcept([](U &&x) -> T { return static_cast<U &&>(x); }(static_cast<U &&>(x))))
    {
        return static_cast<U &&>(x);
    }
};
template <class T>
constexpr inline caster<T> cast{};

//
// defer
//
namespace impl
{
struct deferty {
    template <class F>
    constexpr auto operator=(F &&f) noexcept
    {
        struct R {
            F f;
            ~R() { f(); }
        };
        return R{static_cast<F &&>(f)};
    }
};
} // namespace impl
#define DEFER const auto BOOST_PP_CAT(__defer_, __LINE__) = jutil::impl::deferty{} =

//
// string
//
template <class, std::size_t>
struct string;
template <std::size_t... Is, std::size_t N>
struct string<std::index_sequence<Is...>, N> : std::array<char, N> {
    consteval string(const char (&cs)[N + 1]) noexcept : std::array<char, N>{cs[Is]...} {}
};
template <std::size_t N>
string(const char (&cs)[N]) -> string<std::make_index_sequence<N - 1>, N - 1>;

//
// IF
//

#define JUTIL_IF(E, T, F, ...)                                                                     \
    [__VA_ARGS__]<class BOOST_PP_CAT(IfT, __LINE__), class BOOST_PP_CAT(IfF, __LINE__)>(           \
        const auto BOOST_PP_CAT(ife, __LINE__),                                                    \
        [[maybe_unused]] BOOST_PP_CAT(IfT, __LINE__) && BOOST_PP_CAT(ift, __LINE__),               \
        [[maybe_unused]] BOOST_PP_CAT(IfF, __LINE__) && BOOST_PP_CAT(iff, __LINE__)) {             \
        if constexpr (BOOST_PP_CAT(ife, __LINE__))                                                 \
            return static_cast<BOOST_PP_CAT(IfT, __LINE__) &&>(BOOST_PP_CAT(ift, __LINE__));       \
        else                                                                                       \
            return static_cast<BOOST_PP_CAT(IfF, __LINE__) &&>(BOOST_PP_CAT(iff, __LINE__));       \
    }(std::bool_constant<E>{}, T, F)

//
// ENUM
//
#define JUTIL_ENUM(N, Var, Expr, ...)                                                              \
    ([__VA_ARGS__]<std::size_t... Var>(std::index_sequence<Var...>) -> decltype(auto) {            \
        return Expr;                                                                               \
    })(std::make_index_sequence<(N)>{})

//
// FREF
//
#define FREF(F, ...)                                                                               \
    []<class... BOOST_PP_CAT(Ts, __LINE__)>(                                                       \
        BOOST_PP_CAT(Ts, __LINE__) &&                                                              \
        ...BOOST_PP_CAT(xs,                                                                        \
                        __LINE__)) noexcept(noexcept(F(__VA_ARGS__                                 \
                                                           __VA_OPT__(, ) static_cast<             \
                                                               BOOST_PP_CAT(Ts, __LINE__) &&>(     \
                                                               BOOST_PP_CAT(xs, __LINE__))...)))   \
        -> decltype(F(__VA_ARGS__ __VA_OPT__(, ) static_cast<BOOST_PP_CAT(Ts, __LINE__) &&>(       \
            BOOST_PP_CAT(xs, __LINE__))...))                                                       \
        requires(requires {                                                                        \
            F(__VA_ARGS__ __VA_OPT__(, ) static_cast<BOOST_PP_CAT(Ts, __LINE__) &&>(               \
                BOOST_PP_CAT(xs, __LINE__))...);                                                   \
        })                                                                                         \
    {                                                                                              \
        return F(__VA_ARGS__ __VA_OPT__(, ) static_cast<BOOST_PP_CAT(Ts, __LINE__) &&>(            \
            BOOST_PP_CAT(xs, __LINE__))...);                                                       \
    }

template <auto A, auto B>
    requires std::integral<decltype(A + B)>
constexpr inline auto iota = []<auto... xs>(
    std::integer_sequence<decltype(A + B), xs...>) -> std::array<decltype(A + B), sizeof...(xs)>
{
    return std::array{(A + xs)...};
}
(std::make_integer_sequence<decltype(A + B), B - A>{});

#define NO_COPY_MOVE(S)                                                                            \
    S(const S &)            = delete;                                                              \
    S(S &&)                 = delete;                                                              \
    S &operator=(const S &) = delete;                                                              \
    S &operator=(S &&)      = delete
#define NO_DEF_CTORS(S)                                                                            \
    S()          = delete;                                                                         \
    S(const S &) = delete;                                                                         \
    S(S &&)      = delete;

} // namespace jutil

#include "lmacro.inl"
