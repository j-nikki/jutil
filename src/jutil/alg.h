#pragma once

#include <algorithm>
#include <limits>
#include <memory>
#include <ranges>
#include <span>

#include "core.h"

#include "lmacro.inl"

namespace jutil
{
namespace sr = std::ranges;
namespace sv = std::views;

//
// call
//
template <callable<> F>
constexpr call_result<F &>
call_until(F &&f, std::predicate<call_result<F &>> auto pr) noexcept(noexcept(pr(f())))
{
    for (;;)
        if (decltype(auto) res = f(); pr(res)) return static_cast<decltype(res)>(res);
}
template <callable<> F>
constexpr call_result<F &>
call_while(F &&f, std::predicate<call_result<F &>> auto pr) noexcept(noexcept(pr(f())))
{
    for (;;)
        if (decltype(auto) res = f(); !pr(res)) return static_cast<decltype(res)>(res);
}
template <callable<> F>
constexpr void call_n(F &&f, std::integral auto n) noexcept(noexcept(f()))
{
    while (n--)
        f();
}
template <sr::input_range R, callable<sr::range_value_t<R>> F>
constexpr void call_with(F &&f, R &&r) noexcept(noexcept(f(*sr::begin(r))))
{
    for (auto &&x : static_cast<R &&>(r))
        f(static_cast<decltype(x) &&>(x));
}

//
// swap
//
struct iter_swap_t {
    JUTIL_CI void operator()(auto a, auto b) const noexcept(noexcept(sr::iter_swap(a, b)))
    {
        sr::iter_swap(a, b);
    }
};
constexpr inline iter_swap_t iter_swap{};

//
// slice
//
constexpr inline auto npos = std::numeric_limits<std::ptrdiff_t>::max();
template <std::ptrdiff_t I, std::ptrdiff_t J, std::size_t N>
constexpr inline auto slc_a = J == npos ? 0 : I;
template <std::ptrdiff_t I, std::ptrdiff_t J, std::size_t N>
constexpr inline auto slc_b = J == npos ? (I < 0 ? N + I : I) : (J < 0 ? N + J : J);
template <std::ptrdiff_t I, std::ptrdiff_t J, class T, std::size_t N>
using slcty = std::span<T, slc_b<I, J, N> - slc_a<I, J, N>>;
template <std::ptrdiff_t I, std::ptrdiff_t J = npos, class T, std::size_t N>
JUTIL_CI slcty<I, J, const T, N> slice(const std::array<T, N> &arr) noexcept
{
    return slcty<I, J, const T, N>{&arr[slc_a<I, J, N>], &arr[slc_b<I, J, N>]};
}
template <std::ptrdiff_t I, std::ptrdiff_t J = npos, class T, std::size_t N>
JUTIL_CI slcty<I, J, T, N> slice(std::array<T, N> &arr) noexcept
{
    return slcty<I, J, T, N>{&arr[slc_a<I, J, N>], &arr[slc_b<I, J, N>]};
}

JUTIL_CI std::size_t operator""_uz(unsigned long long int x) noexcept
{
    return static_cast<std::size_t>(x);
}

template <class>
struct is_sized_span : std::false_type {};
template <class T, std::size_t N>
struct is_sized_span<std::span<T, N>> : std::bool_constant<N != std::dynamic_extent> {};

template <class T>
concept constant_sized = (std::is_bounded_array_v<T> || is_sized_span<T>::value ||
                          requires { std::tuple_size<T>::value; });
template <class R>
concept constant_sized_input_range = sr::input_range<R> && constant_sized<std::remove_cvref_t<R>>;
template <class R, class T>
concept constant_sized_output_range =
    sr::output_range<R, T> && constant_sized<std::remove_cvref_t<R>>;
template <class T>
concept borrowed_constant_sized_range =
    sr::borrowed_range<T> && constant_sized<std::remove_cvref_t<T>>;
template <class T>
concept borrowed_input_range = sr::borrowed_range<T> && sr::input_range<T>;
template <class T>
concept sized_input_range = sr::input_range<T> && (requires(T r) { sr::size(r); });
template <class T>
concept sized_contiguous_range = sr::contiguous_range<T> && (requires(T r) { sr::size(r); });
template <class R, class T>
concept sized_output_range = sr::output_range<R, T> && (requires(R r) { sr::size(r); });

template <class T_>
constexpr inline std::size_t csr_sz = [] {
    using T = std::remove_cvref_t<T_>;
    if constexpr (std::is_bounded_array_v<T>) {
        return std::extent_v<T>;
    } else if constexpr (is_sized_span<T>::value)
        return []<class T, std::size_t N>(std::type_identity<std::span<T, N>>) {
            return N;
        }(std::type_identity<T>{});
    else return std::tuple_size_v<T>;
}();

static_assert(constant_sized_input_range<const std::array<int, 1> &>);
static_assert(constant_sized_input_range<const std::span<int, 1> &>);
static_assert(constant_sized_input_range<const int (&)[5]>);

template <sr::random_access_range R, class Comp, class Proj>
    requires(std::sortable<sr::iterator_t<R>, Comp, Proj> && requires(R r) { sr::size(r); })
JUTIL_CI sr::iterator_t<R> sort_until_snt(R &&r, Comp comp, Proj proj, auto pred)
{
    const auto l = sr::next(sr::begin(r), sr::size(r) - 1);
    for (auto it = sr::begin(r);; ++it, assert(it != sr::end(r)))
        if (sr::nth_element(it, it, l, comp, proj); pred(*it)) return it;
}
template <sr::random_access_range R, class Comp, class Proj, class Pred, class Snt>
    requires(std::sortable<sr::iterator_t<R>, Comp, Proj> && requires(R r) { sr::size(r); })
JUTIL_CI sr::iterator_t<R> sort_until_snt(R &&r, Comp comp, Proj proj, Pred pred, Snt &&snt)
{
    *sr::next(sr::begin(r), sr::size(r) - 1) = static_cast<Snt &&>(snt);
    return sort_until_snt(r, comp, proj, pred, snt);
}

//
// counts
//
template <std::size_t NTypes, class TCnt = std::size_t, sr::input_range R,
          class Proj = std::identity>
[[nodiscard]] JUTIL_CI std::array<TCnt, NTypes>
counts(R &&r, Proj proj = {}) noexcept(noexcept(proj(*sr::begin(r))))
{
    std::array<TCnt, NTypes> cnts{};
    for (auto &x : r)
        ++cnts[proj(x)];
    return cnts;
}

//
// find_unrl
//
namespace impl
{
template <std::size_t I>
JUTIL_CI auto find_if_unrl(auto &r, auto it, auto pred) noexcept(noexcept(pred(*sr::next(it))))
{
    if constexpr (!I) return sr::end(r);
    else if (pred(*it)) return it;
    else return find_if_unrl<I - 1>(r, sr::next(it), pred);
}
template <std::size_t I>
JUTIL_CI auto find_unrl(auto &r, auto it, const auto &x) noexcept(noexcept(*sr::next(it) == x))
{
    if constexpr (!I) return sr::end(r);
    else if (*it == x) return it;
    else return find_unrl<I - 1>(r, sr::next(it), x);
}
} // namespace impl
template <borrowed_constant_sized_range R, class Pred>
[[nodiscard]] JUTIL_CI sr::iterator_t<R>
find_if_unrl(R &&r,
             Pred pred) noexcept(noexcept(impl::find_if_unrl<csr_sz<R>>(r, sr::begin(r), pred)))
{
    return impl::find_if_unrl<csr_sz<R>>(r, sr::begin(r), pred);
}
template <borrowed_constant_sized_range R>
[[nodiscard]] JUTIL_CI sr::iterator_t<R>
find_unrl(R &&r, const auto &x) noexcept(noexcept(impl::find_unrl<csr_sz<R>>(r, sr::begin(r), x)))
{
    return impl::find_unrl<csr_sz<R>>(r, sr::begin(r), x);
}
template <borrowed_constant_sized_range R>
[[nodiscard]] JUTIL_CI std::size_t
find_unrl_idx(R &&r, const auto &x) noexcept(noexcept(sr::distance(sr::begin(r), find_unrl(r, x))))
{
    return static_cast<std::size_t>(sr::distance(sr::begin(r), find_unrl(r, x)));
}
template <borrowed_constant_sized_range R>
[[nodiscard]] JUTIL_CI std::size_t
find_if_unrl_idx(R &&r,
                 auto pred) noexcept(noexcept(sr::distance(sr::begin(r), find_if_unrl(r, pred))))
{
    return static_cast<std::size_t>(sr::distance(sr::begin(r), find_if_unrl(r, pred)));
}

//
// map
//
template <std::size_t N, std::size_t Pad = 0, bool InitPad = true, sr::input_range R,
          callable<std::size_t, sr::range_reference_t<R>> F>
[[nodiscard]] JUTIL_CI auto map_n(R &&r, F f) noexcept(
    noexcept(std::decay_t<decltype(f(0_uz, *sr::begin(r)))>{f(0_uz, *sr::begin(r))}))
    -> std::array<std::decay_t<decltype(f(0_uz, *sr::begin(r)))>, N + Pad>
{
    using OEl = std::decay_t<decltype(f(0_uz, *sr::begin(r)))>;
    if consteval {
        std::array<OEl, N + Pad> res{};
        auto i = 0_uz;
        for (sr::range_reference_t<R> x : static_cast<R &&>(r))
            res[i] = f(i, x), ++i;
        return res;
    } else {
        std::array<OEl, N + Pad> res;
        auto i = 0_uz;
        for (sr::range_reference_t<R> x : static_cast<R &&>(r))
            new (&res[i]) OEl{f(i, x)}, ++i;
        if constexpr (InitPad)
            std::uninitialized_default_construct_n(std::next(res.begin(), N), Pad);
        return res;
    }
}
template <std::size_t N, std::size_t Pad = 0, bool InitPad = true, sr::input_range R,
          callable<sr::range_reference_t<R>> F = std::identity>
[[nodiscard]] JUTIL_CI auto
map_n(R &&r,
      F f = {}) noexcept(noexcept(std::decay_t<decltype(f(*sr::begin(r)))>{f(*sr::begin(r))}))
    -> std::array<std::decay_t<decltype(f(*sr::begin(r)))>, N + Pad>
{
    using OEl = std::decay_t<decltype(f(*sr::begin(r)))>;
    if consteval {
        std::array<OEl, N + Pad> res{};
        auto i = 0_uz;
        for (sr::range_reference_t<R> x : static_cast<R &&>(r))
            res[i++] = f(x);
        return res;
    } else {
        std::array<OEl, N + Pad> res;
        auto i = 0_uz;
        for (sr::range_reference_t<R> x : static_cast<R &&>(r))
            new (&res[i++]) OEl{f(x)};
        if constexpr (InitPad)
            std::uninitialized_default_construct_n(std::next(res.begin(), N), Pad);
        return res;
    }
}
template <std::size_t Pad = 0, bool InitPad = true, constant_sized_input_range R>
[[nodiscard]] JUTIL_CI auto
map(R &&r, auto f) noexcept(noexcept(map_n<csr_sz<R>, Pad, InitPad>(static_cast<R &&>(r), f)))
    -> decltype(map_n<csr_sz<R>, Pad, InitPad>(static_cast<R &&>(r), f))
{
    return map_n<csr_sz<R>, Pad, InitPad>(static_cast<R &&>(r), f);
}

template <sr::range R, class InitT = sr::range_value_t<R>>
[[nodiscard]] constexpr InitT sum(R &&r, InitT init = {}) noexcept
{
    for (const auto &x : static_cast<R &&>(r))
        init += x;
    return init;
}

//
// transform_while
//
template <std::input_iterator It, class UnaryOp,
          std::output_iterator<call_result<UnaryOp &, std::iter_reference_t<It>>> It2,
          std::predicate<std::iter_reference_t<It>> Pred>
constexpr It2
transform_while(It f, It l, It2 f2, UnaryOp transf,
                Pred pred) noexcept(noexcept(*++f2 = transf(*++f)) && noexcept(pred(*f)))
{
    for (; f != l && pred(*f); ++f, ++f2)
        *f2 = transf(*f);
    return f2;
}

//
// transform_until
//
template <std::random_access_iterator It, class UnaryOp,
          std::output_iterator<call_result<UnaryOp &, std::iter_reference_t<It>>> It2>
constexpr It2 transform_until_snt(It f, const It l, It2 f2, UnaryOp transf, const auto &x) noexcept(
    noexcept(*++f2 = transf(*++f)) && noexcept(f[l - f - 1] = x))
{
    f[l - f - 1] = x;
    for (; *f != x; ++f, ++f2)
        *f2 = transf(*f);
    return f2;
}
template <std::input_iterator It, class UnaryOp,
          std::output_iterator<call_result<UnaryOp &, std::iter_reference_t<It>>> It2>
constexpr It2 transform_always_until(
    It f, const It l, It2 f2, UnaryOp transf,
    const auto &x) noexcept(noexcept(*++f2 = transf(*++f)) && noexcept(f[l - f - 1] = x))
{
    for (; *f != x; ++f, ++f2)
        CHECK(f != l), *f2 = transf(*f);
    return f2;
}

//
// construct
//
template <sr::input_range R, class V = sr::range_value_t<R>, class Ctor = decltype(L(new(x) V{}))>
    requires std::is_constructible_v<V> //
JUTIL_CI void construct(R &&r, Ctor &&c = {}) noexcept(std::is_nothrow_constructible_v<V>)
{
    for (auto &x : r)
        c(&x);
}

//
// has
//
template <sr::input_range R, class TVal, class Proj = std::identity>
[[nodiscard]] constexpr bool has(R &&r, const TVal &val, Proj proj = {})
{
    return sr::find(r, val, proj) != sr::end(r);
}
template <constant_sized_input_range R, class TVal, class Proj = std::identity>
[[nodiscard]] constexpr bool has_unrl(R &&r, const TVal &val, Proj proj = {})
{
    return find_unrl(r, val) != sr::end(r);
}

//
// max heap
//
template <std::random_access_iterator I, std::sentinel_for<I> S, class Comp = sr::less,
          class Proj = std::identity, class IterSwap = iter_swap_t>
    requires std::sortable<I, Comp, Proj>
constexpr I
push_heap(I f, S l, Comp comp = {}, Proj proj = {},
          IterSwap is = {}) noexcept(noexcept(comp(proj(f[0]), proj(f[0]))) && noexcept(is(f, f)))
{
    auto xi = (l - f) - 1;
    for (auto upi = xi / 2;; (xi = upi, upi = xi / 2))
        if (!xi || comp(proj(f[xi]), proj(f[upi]))) return f + xi;
        else is(f + upi, f + xi);
}

//
// eytzinger-indexed heap
//
template <class IdxTy = std::size_t, std::random_access_iterator I, std::sentinel_for<I> S,
          class Comp = sr::less, class Proj = std::identity>
[[nodiscard]] JUTIL_CI bool
is_heap_eytz(I f, S l, Comp comp = {},
             Proj proj = {}) noexcept(noexcept(comp(proj(f[0]), proj(f[0]))))
{
    const auto n = static_cast<IdxTy>(l - f);
    for (IdxTy i = 1; i / 2 < n; ++i) {
        const auto li = i * 2, ri = i * 2 + 1;
        if (li < n && !comp(proj(f[li]), proj(f[i]))) return false;
        if (ri < n && !comp(proj(f[i]), proj(f[ri]))) return false;
    }
    return true;
}
template <class RetTy = std::size_t, bool Trim = true, std::random_access_iterator I,
          std::sentinel_for<I> S, class Comp = sr::less, class Proj = std::identity>
[[nodiscard]] JUTIL_CI RetTy lower_bound_eytz_idx(I f, S l, const auto &x, Comp comp = {},
                                                  Proj proj = {}) noexcept(noexcept(comp(proj(f[1]),
                                                                                         x)))
{
    CHECK(is_heap_eytz(f, l, comp, proj));
    const auto n = static_cast<RetTy>(l - f);
    RetTy i      = 1;
    while (i < n) {
        prefetch(f + i * (hardware_destructive_interference_size / sizeof(f[i])));
        i = i * 2 + comp(proj(f[i]), x);
    }
    return Trim ? CHECK(i >> __builtin_ffs(~i), > 0, < n) : i;
}
template <class IdxTy = std::size_t, std::random_access_iterator I, std::sentinel_for<I> S,
          class Comp = sr::less, class Proj = std::identity>
[[nodiscard]] JUTIL_CI I lower_bound_eytz(I f, S l, const auto &x, Comp comp = {},
                                          Proj proj = {}) noexcept(noexcept(comp(proj(f[1]),
                                                                                 proj(f[1]))))
{
    return f + lower_bound_eytz_idx<IdxTy>(f, l, x, comp, proj);
}
JUTIL_PUSH_DIAG(JUTIL_WNO_PARENTHESES JUTIL_WNO_SEQUENCE)
template <class IdxTy = std::size_t, std::size_t DD = 1, std::random_access_iterator I,
          std::sentinel_for<I> S, class Comp = sr::less, class Proj = std::identity>
JUTIL_CI void push_eytz(I f, S l, Comp comp = {},
                        Proj proj = {}) noexcept(noexcept(comp(proj(f[1]), proj(f[1]))))
{
    const auto xi = static_cast<IdxTy>(l - f) - 1;
    if (xi == 1) return;
    const auto hm1         = static_cast<IdxTy>(bsr(to_unsigned(xi - 1)));
    const IdxTy ctzxormsk1 = ~(2u << hm1), ctzxormsk2 = ~(1u << hm1);
    for (;;) {
        CHECK(is_heap_eytz(f, f + xi, comp, proj));
        IdxTy lb = 1;
        duffs<DD>(hm1, [&] { lb = (lb * 2) + comp(proj(f[lb]), proj(f[xi])); });
        const auto lblt = lb < xi;
        lb              = !lblt ? lb : lb * 2 + comp(proj(f[lb]), proj(f[xi]));
        if (lb == xi) break;
        const auto ctzxormsk = lblt ? ctzxormsk1 : ctzxormsk2;
        const auto nz        = static_cast<IdxTy>(ctz(to_unsigned((lb & 1) ? lb ^ ctzxormsk : lb)));
        std::iter_value_t<I> tmp{sr::iter_move(f + (lb >> nz))};
        duffs<DD>(nz - 1, [&, i = nz] mutable { f[lb >> i--] = sr::iter_move(f + (lb >> i - 1)); });
        f[lb >> 1] = sr::iter_move(f + xi);
        f[xi]      = std::move(tmp);
    }
}
JUTIL_POP_DIAG()

//
// find_always
//
template <sr::random_access_range R, class T, class Proj = std::identity>
    requires std::is_lvalue_reference_v<call_result<Proj &, sr::range_reference_t<R>>>
[[nodiscard]] JUTIL_CI std::size_t find_always_idx(R &r, const T &x, Proj proj = {})
{
    const auto it = sr::begin(r);
    for (std::size_t i = 0;; ++i, assert(i != sr::size(r)))
        if (proj(it[i]) == x) return i;
}
template <std::input_iterator It, class T, class Proj = std::identity>
    requires std::is_lvalue_reference_v<call_result<Proj &, std::iter_reference_t<It>>>
[[nodiscard]] JUTIL_CI It find_always(It f, [[maybe_unused]] const It l, const T &x, Proj proj = {})
{
    while (proj(*f) != x)
        CHECK(f != l), ++f;
    return f;
}
template <sr::input_range R, class T, class Proj = std::identity>
    requires std::is_lvalue_reference_v<call_result<Proj &, sr::range_reference_t<R>>>
[[nodiscard]] JUTIL_CI sr::iterator_t<R> find_always(R &r, const T &x, Proj proj = {})
{
    return find_always(sr::begin(r), x, proj);
}
template <std::random_access_iterator It, class Pred, class Proj = std::identity>
[[nodiscard]] JUTIL_CI std::size_t find_if_always_idx(It f, [[maybe_unused]] const It l, Pred pred,
                                                      Proj proj = {})
{
    for (std::size_t i = 0;; ++i)
        if (CHECK(i != static_cast<std::size_t>(sr::distance(f, l))), pred(proj(f[i]))) return i;
}
template <sr::random_access_range R, class Pred, class Proj = std::identity>
[[nodiscard]] JUTIL_CI std::size_t find_if_always_idx(R &r, Pred pred, Proj proj = {})
{
    return find_if_always_idx(sr::begin(r), sr::end(r), pred, proj);
}
template <std::input_iterator It, class Proj = std::identity>
[[nodiscard]] JUTIL_CI It find_if_always(It f, [[maybe_unused]] const It l, auto pred,
                                         Proj proj = {})
{
    while (!pred(proj(*CHECK(f, != l))))
        ++f;
    return f;
}
template <borrowed_input_range R, class Proj = std::identity>
[[nodiscard]] JUTIL_CI sr::iterator_t<R> find_if_always(R &&r, auto pred, Proj proj = {})
{
    return find_if_always(sr::begin(r), sr::end(r), pred, proj);
}

//
// find_snt
//
template <std::random_access_iterator It, class T, class Proj = std::identity>
    requires std::is_lvalue_reference_v<call_result<Proj &, std::iter_reference_t<It>>>
[[nodiscard]] JUTIL_CI It find_snt(It f, const It l, const T &x, Proj proj = {})
{
    proj(f[l - f - 1]) = x;
    while (proj(*f) != x)
        ++f;
    return f;
}
template <std::random_access_iterator It, class T, class Proj = std::identity>
    requires std::is_lvalue_reference_v<call_result<Proj &, std::iter_reference_t<It>>>
[[nodiscard]] JUTIL_CI It find_if_snt(It f, const It l, auto pred, const T &x, Proj proj = {})
{
    proj(f[l - f - 1]) = x;
    while (!pred(proj(*f)))
        ++f;
    return f;
}
template <sr::random_access_range R, class T, class Proj = std::identity>
    requires std::is_lvalue_reference_v<call_result<Proj &, sr::range_reference_t<R>>>
[[nodiscard]] JUTIL_CI sr::iterator_t<R &> find_snt(R &r, const T &x, Proj proj = {})
{
    return find_snt(sr::begin(r), sr::end(r), x, proj);
}
template <sr::random_access_range R, class T, class Proj = std::identity>
    requires std::is_lvalue_reference_v<call_result<Proj &, sr::range_reference_t<R>>>
[[nodiscard]] JUTIL_CI std::size_t find_snt_idx(R &r, const T &x, Proj proj = {})
{
    proj(sr::begin(r)[sr::size(r) - 1]) = x;
    return find_always_idx(r, x, proj);
}

//
// each
//
namespace detail
{
struct each_caller_ty {
    template <class T, class U, class V, callable<T &&, U &&, V &&> F>
    JUTIL_CI void operator()(T &&x, U, V, F &&f) const
        noexcept(noexcept(static_cast<F &&>(f)(static_cast<T &&>(x), U{}, V{})))
    {
        static_cast<F &&>(f)(static_cast<T &&>(x), U{}, V{});
    }
    template <class T, class U, class V, callable<T &&, U &&> F>
    JUTIL_CI void operator()(T &&x, U, V, F &&f) const
        noexcept(noexcept(static_cast<F &&>(f)(static_cast<T &&>(x), U{})))
    {
        static_cast<F &&>(f)(static_cast<T &&>(x), U{});
    }
    template <class T, class U, class V, callable<T &&> F>
    JUTIL_CI void operator()(T &&x, U, V, F &&f) const
        noexcept(noexcept(static_cast<F &&>(f)(static_cast<T &&>(x))))
    {
        static_cast<F &&>(f)(static_cast<T &&>(x));
    }
};
constexpr inline each_caller_ty each_caller;
template <class T, std::size_t... Is>
JUTIL_CI void each_impl(T &&xs, auto &&f, std::index_sequence<Is...>) noexcept(noexcept(
    (each_caller(std::get<Is>(static_cast<T &&>(xs)), std::integral_constant<std::size_t, Is>{},
                 std::bool_constant<Is == sizeof...(Is) - 1>{}, f),
     ...)))
{
    (each_caller(std::get<Is>(static_cast<T &&>(xs)), std::integral_constant<std::size_t, Is>{},
                 std::bool_constant<Is == sizeof...(Is) - 1>{}, f),
     ...);
};
} // namespace detail
template <class T, class F>
JUTIL_CI void each(T &&xs, F &&f) noexcept(noexcept(
    detail::each_impl(static_cast<T &&>(xs), static_cast<F &&>(f),
                      std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<T>>>{})))
{
    detail::each_impl(static_cast<T &&>(xs), static_cast<F &&>(f),
                      std::make_index_sequence<std::tuple_size_v<std::remove_cvref_t<T>>>{});
}

//
// fold
//
template <class InitT, sr::input_range R, class F>
    requires(is_callable_r<InitT, F, InitT, sr::range_reference_t<R>>)
[[nodiscard]] JUTIL_INLINE InitT fold(R &&r, InitT init,
                                      F f) noexcept(noexcept(init = f(init, *sr::begin(r))))
{
    for (sr::range_reference_t<R> x : r)
        init = f(init, x);
    return init;
}
} // namespace jutil

#include "lmacro.inl"
