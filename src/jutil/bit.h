#pragma once

#if !JUTIL_HOSTED
// This is a hack to prevent the compiler from including mm_malloc.h
#define _MM_MALLOC_H_INCLUDED
#endif

#include <algorithm>
#include <boost/preprocessor/comma_if.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <climits>
#include <immintrin.h>
#include <stdio.h>

#include "core.h"
#include "macro.h"

namespace jutil
{
//
// intrinsics
//
inline namespace intrin
{
#define JUTIL_i_enum(r, _, i, x)     BOOST_PP_COMMA_IF(i) x BOOST_PP_CAT(arg, i)
#define JUTIL_i_enum_fwd(r, _, i, x) BOOST_PP_COMMA_IF(i) BOOST_PP_CAT(arg, i)
#define JUTIL_intrin_exp(n, in, s)                                                                 \
    JUTIL_INLINE auto n(BOOST_PP_SEQ_FOR_EACH_I(JUTIL_i_enum, ~, s)) noexcept(                     \
        noexcept(in(BOOST_PP_SEQ_FOR_EACH_I(JUTIL_i_enum_fwd, ~, s))))                             \
        ->decltype(in(BOOST_PP_SEQ_FOR_EACH_I(JUTIL_i_enum_fwd, ~, s)))                            \
    {                                                                                              \
        return in(BOOST_PP_SEQ_FOR_EACH_I(JUTIL_i_enum_fwd, ~, s));                                \
    }
#define JUTIL_INTRIN(name, iname, ...)                                                             \
    JUTIL_intrin_exp(name, iname, BOOST_PP_VARIADIC_TO_SEQ(__VA_ARGS__))
// clang-format off
[[nodiscard]] JUTIL_INTRIN(ffs, __builtin_ffs, int)
[[nodiscard]] JUTIL_INTRIN(ffs, __builtin_ffsl, long)
[[nodiscard]] JUTIL_INTRIN(ffs, __builtin_ffsll, long long)
[[nodiscard]] JUTIL_INTRIN(clz, __builtin_clz, unsigned int)
[[nodiscard]] JUTIL_INTRIN(ctz, __builtin_ctz, unsigned int)
[[nodiscard]] JUTIL_INTRIN(clrsb, __builtin_clrsb, int)
[[nodiscard]] JUTIL_INTRIN(clrsb, __builtin_clrsbl, long)
[[nodiscard]] JUTIL_INTRIN(clrsb, __builtin_clrsbll, long long)
[[nodiscard]] JUTIL_INTRIN(popcount, __builtin_popcount, unsigned int)
[[nodiscard]] JUTIL_INTRIN(popcount, __builtin_popcountl, unsigned long)
[[nodiscard]] JUTIL_INTRIN(popcount, __builtin_popcountll, unsigned long long)
[[nodiscard]] JUTIL_INTRIN(parity, __builtin_parity, unsigned int)
[[nodiscard]] JUTIL_INTRIN(parity, __builtin_parityl, unsigned long)
[[nodiscard]] JUTIL_INTRIN(parity, __builtin_parityll, unsigned long long)
[[nodiscard]] JUTIL_INTRIN(clz, __builtin_clzl, unsigned long)
[[nodiscard]] JUTIL_INTRIN(clz, __builtin_clzll, unsigned long long)
[[nodiscard]] JUTIL_INTRIN(ctz, __builtin_ctzl, unsigned long)
[[nodiscard]] JUTIL_INTRIN(ctz, __builtin_ctzll, unsigned long long)
[[nodiscard]] JUTIL_INTRIN(powi, __builtin_powi, double, int)
[[nodiscard]] JUTIL_INTRIN(powi, __builtin_powif, float, int)
[[nodiscard]] JUTIL_INTRIN(powi, __builtin_powil, long double, int)
[[nodiscard]] JUTIL_INTRIN(bswap, __builtin_bswap16, uint16_t)
[[nodiscard]] JUTIL_INTRIN(bswap, __builtin_bswap32, uint32_t)
[[nodiscard]] JUTIL_INTRIN(bswap, __builtin_bswap64, uint64_t)
[[nodiscard]] JUTIL_INTRIN(bsr, (CHAR_BIT * sizeof(int) - 1) ^ __builtin_clz, unsigned int)
[[nodiscard]] JUTIL_INTRIN(bsr, (CHAR_BIT * sizeof(long) - 1) ^ __builtin_clzl, unsigned long)
[[nodiscard]] JUTIL_INTRIN(bsr, (CHAR_BIT * sizeof(long long) - 1) ^ __builtin_clzl, unsigned long long)
JUTIL_INTRIN(prefetch, __builtin_prefetch, const void *)

#define VECINS(N) \
[[nodiscard]] JUTIL_INTRIN(shuffle_epi8, CAT(CAT(_mm, N), _shuffle_epi8), CAT(CAT(__m, N),i), CAT(CAT(__m, N),i)) \
[[nodiscard]] JUTIL_INTRIN(cmpeq_epi8_mask, CAT(CAT(_mm, N),_cmpeq_epi8_mask), CAT(CAT(__m, N),i), CAT(CAT(__m, N),i))
VECINS(256)
VECINS(512)
// clang-format on
} // namespace intrin

//
// load
//
template <trivially_copyable T>
[[nodiscard]] JUTIL_CI T loadu(const char *p) noexcept
{
    if consteval {
        alignas(T) std::array<char, sizeof(T)> buf{};
        std::copy_n(p, sizeof(T), buf.begin());
        return std::bit_cast<T>(buf);
    } else {
        alignas(T) std::array<char, sizeof(T)> buf;
        std::copy_n(p, sizeof(T), buf.begin());
        return std::bit_cast<T>(buf);
    }
}

//
// store
//
template <contiguous_output_iterator<char> O>
JUTIL_CI O storeu(const trivially_copyable auto &x, O d_it) noexcept
{
    if consteval {
        const auto buf = std::bit_cast<std::array<char, sizeof(x)>>(x);
        std::copy_n(buf.begin(), sizeof(x), d_it);
    } else {
        memcpy(d_it, std::addressof(x), sizeof(x));
    }
    return d_it + sizeof(x);
}

//
// opaque
//
template <class T, callable<T &> F, class R = decltype(f(std::declval<T &>()))>
R opaque_visit(char *p, callable<T &> auto &&f) noexcept(noexcept(f(std::declval<T &>())))
{
    T x   = loadu<T>(p);
    R res = f(x);
    storeu(p, x);
    return res;
}

//
// print_{bytes,bits}
//
namespace detail
{
void fprint_bytes(FILE *const f, const char *const p, const std::size_t n) noexcept;
void fprint_bits(FILE *const f, const char *const p, const std::size_t n) noexcept;
} // namespace detail
JUTIL_INLINE void fprint_bytes(FILE *const f, auto &&x) noexcept
{
    char buf[sizeof(x)];
    memcpy(buf, &x, sizeof(x));
    detail::fprint_bytes(f, buf, sizeof(x));
}
template <class T>
JUTIL_INLINE void print_bytes(T &&x) noexcept
{
    fprint_bytes(stdout, static_cast<T &&>(x));
}
JUTIL_INLINE void fprint_bits(FILE *const f, auto &&x) noexcept
{
    char buf[sizeof(x)];
    memcpy(buf, &x, sizeof(x));
    detail::fprint_bits(f, buf, sizeof(x));
}
template <class T>
JUTIL_INLINE void print_bits(T &&x) noexcept
{
    fprint_bits(stdout, static_cast<T &&>(x));
}
} // namespace jutil
