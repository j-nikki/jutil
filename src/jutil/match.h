#pragma once

#include "bit.h"
#include "data.h"
#include "meta.h"

namespace jutil
{
struct string_view {
    char *p;
    std::size_t n;
    JUTIL_CI char *begin() noexcept { return p; }
    JUTIL_CI char *end() noexcept { return p + n; }
    JUTIL_CI const char *begin() const noexcept { return p; }
    JUTIL_CI const char *end() const noexcept { return p + n; }
};

namespace detail
{
template <auto Fmt>
JUTIL_CI auto match(char *it, char *const l, auto &&...xs) noexcept
{
#define MRET(E, Drop, It, ...)                                                                     \
    do {                                                                                           \
        static constexpr auto d   = Drop;                                                          \
        static constexpr auto opt = Fmt.size() > d && Fmt[d] == '?';                               \
        if constexpr (opt) {                                                                       \
            return match<drop<d + 1>(Fmt)>(                                                        \
                (E) ? (It) : it, l,                                                                \
                std::move(xs)... __VA_OPT__(, (E) ? std::optional{__VA_ARGS__}                     \
                                                  : decltype(std::optional{__VA_ARGS__}){}));      \
        } else if (E) {                                                                            \
            return match<drop<d>(Fmt)>(It, l, std::move(xs)... __VA_OPT__(, __VA_ARGS__));         \
        } else {                                                                                   \
            auto res    = decltype(match<drop<d>(Fmt)>(                                            \
                It, l, std::move(xs)... __VA_OPT__(, __VA_ARGS__))){};                          \
            get<0>(res) = nullptr;                                                                 \
            return res;                                                                            \
        }                                                                                          \
    } while (0)
    if constexpr (Fmt.empty()) {
        return jutil::tuple{std::move(it), std::move(xs)...};
    } else if constexpr (Fmt[0] == '$') {
        MRET(it == l, 1, it);
    } else if constexpr (Fmt[0] == '\\') {
        MRET(it[0] == Fmt[1], 2, it + 1);
    } else if constexpr (Fmt[0] == '%') {
        if constexpr (static constexpr std::size_t i = Fmt[1] - '0'; i < 10) {
            return [&]<std::size_t... Is> [[gnu::always_inline]] (
                       std::index_sequence<Is...>) noexcept {
                MRET(it + i <= l, 2, it + i, std::array{it[Is]...});
            }(std::make_index_sequence<i>{});
        } else if constexpr (Fmt[1] == 's') {
            auto it2 = it;
            *l       = Fmt[2];
            for (; *it2 != Fmt[2]; ++it2)
                ;
            MRET(it2 != l, 2, it2, string_view{it, static_cast<std::size_t>(it2 - it)});
        }
#if JUTIL_HOSTED
#define MCHCNV(X, ...)                                                                             \
    else if constexpr (Fmt[1] == FST X)                                                            \
    {                                                                                              \
        SND X val;                                                                                 \
        auto [it2, ec] = std::from_chars(it, l, val WCOMMA(TAIL(TAIL X)));                         \
        MRET(ec == std::errc{}, 2, const_cast<char *>(it2), auto{val});                            \
    }
        FOR_EACH(MCHCNV, ('d', int), ('u', unsigned int), ('f', float), ('x', unsigned int, 16),
                 ('b', unsigned char, 16))
#endif
        else
        {
            static_assert(false, "bad format");
        }
    } else if constexpr (Fmt[0] == '[') {
        static constexpr auto neg = Fmt[1] == '^';
        static constexpr auto l_  = sr::find(Fmt, ']');
        auto it2                  = it;
        struct range {
            char c;
            unsigned char n;
        };
        static constexpr auto n   = l_ + 1 - Fmt.begin();
        static constexpr auto rgs = [] {
            std::array<range, n> res{};
            res.fill({0, 0});
            auto it_ = res.begin();
            for (std::size_t i = 1 + neg; i < n - 1;) {
                if (Fmt[i] == '\\') *it_++ = {Fmt[i + 1], 1}, i += 2;
                else if (Fmt[i + 1] != '-') *it_++ = {Fmt[i], 1}, i += 1;
                else *it_++ = {Fmt[i], static_cast<unsigned char>(Fmt[i + 2] - Fmt[i] + 1)}, i += 3;
            }
            return res;
        }();
        *l = neg ? rgs[0].c : '\0';
#define JUTIL_mrg()                                                                                \
    (apply(rgs, [&](auto... r) {                                                                   \
         return (... || (static_cast<unsigned char>(*it2 - r.c) < r.n));                           \
     }) == !neg)
        if constexpr (Fmt.size() > n && (l_[1] == '+' || l_[1] == '*')) {
            while (JUTIL_mrg())
                ++it2;
            MRET((it2 - it) >= (l_[1] == '+'), n + 1, it2);
        } else {
            MRET(JUTIL_mrg(), n, it2);
        }
    } else {
        static constexpr auto f2          = sr::find_first_of(Fmt, string{"%$["});
        static constexpr std::size_t nstr = f2 - sr::begin(Fmt);
        static constexpr auto str         = take<nstr>(Fmt);
        MRET(std::equal(it, it + nstr, str.data()), nstr, it + nstr);
    }
}
} // namespace detail
template <string S>
[[nodiscard]] JUTIL_CI auto match(char *f, char *l) noexcept -> decltype(detail::match<S>(f, l))
{
    DEFER[c = *l, l] { *l = c; };
    return detail::match<S>(f, l);
}
template <string S>
[[nodiscard]] JUTIL_CI auto match(sr::contiguous_range auto &&r) noexcept
    -> decltype(detail::match<S>(sr::begin(r), sr::end(r)))
{
    return detail::match<S>(sr::begin(r), sr::end(r));
}
namespace detail
{
template <auto Pre, bool PrNeg, auto S, auto Post, bool PoNeg>
JUTIL_CI void split(char *i, char *l, auto f) noexcept
{
    for (auto j = i; j != l;) {
        if constexpr (!PrNeg)
            while (!get<0>(jutil::match<Pre>(j++, l)))
                ;
        if (const auto m = get<0>(match<S>(j, l)); m)
            if (!!get<0>(jutil::match<Post>(m, l)) ^ PoNeg) f(i, j), i = j = m;
        if constexpr (PrNeg)
            while (get<0>(jutil::match<Pre>(j++, l)))
                ;
    }
    f(i, l);
}
} // namespace detail
template <string S, std::contiguous_iterator I>
JUTIL_CI void split(I i, std::sentinel_for<I> auto l, jutil::callable<I, I> auto f) noexcept
{
    if constexpr (sr::count(S, '?') == 2) {
        static constexpr auto a  = sr::find(S, '?') - S.begin();
        static constexpr auto b  = sr::find(S.begin() + a + 1, S.end(), '?') - S.begin();
        static constexpr auto an = S[0] == '!';
        static constexpr auto bn = S[b + 1] == '!';
        detail::split<drop<an>(take<a>(S)), an, take<b - a - 1>(drop<a + 1>(S)),
                      drop<bn>(drop<b + 1>(S)), bn>(i, l, f);
    } else {
        detail::split<string{"$"}, true, S, string{""}, false>(i, l, f);
    }
}
namespace detail
{
template <string... Ss>
constexpr auto match_idx = [] {
    std::array res{Ss.size()...};
    auto it  = res.begin();
    auto acc = 0uz;
    (((*it = (1uz << (*it + std::exchange(acc, acc + Ss.size() + 1)))), ++it), ...);
    return res;
}();

template <constant I, constant J, class F> // callable<I, J> F>
                                           // requires(!callable<F, std::size_t, typename J::type>)
JUTIL_CI auto match_call(F &&f, I, J) noexcept(noexcept(f(I{}, J{})))
    -> std::conditional_t<std::is_void_v<decltype(f(I{}, J{}))>, std::size_t, decltype(f(I{}, J{}))>
{
    if constexpr (!J::value && !callable<F, I, J>) {
        return J::value;
    } else if constexpr (std::is_void_v<decltype(f(I{}, J{}))>) {
        f(I{}, J{});
        return J::value;
    } else return f(I{}, J{});
}

template <constant I, constant J, class F> // callable<J> F>
                                           // requires(!callable<F, typename J::type>)
JUTIL_CI auto match_call(F &&f, I, J) noexcept(noexcept(f(J{})))
    -> std::conditional_t<std::is_void_v<decltype(f(J{}))>, std::size_t, decltype(f(J{}))>
{
    if constexpr (!J::value && !callable<F, I, J>) {
        return J::value;
    } else if constexpr (std::is_void_v<decltype(f(J{}))>) {
        f(J{});
        return J::value;
    } else return f(J{});
}

template <class J>
JUTIL_CI auto match_call(callable<std::size_t, decltype(J{} + 0)> auto &&f, std::size_t i,
                         J j) noexcept(noexcept(f(i, j + 0)))
    -> std::conditional_t<std::is_void_v<decltype(f(i, j + 0))>, decltype(j + 0),
                          decltype(f(i, j + 0))>
{
    if constexpr (std::is_void_v<decltype(f(i, j + 0))>) {
        f(i, j + 0);
        return j + 0;
    } else return f(i, j + 0);
}

template <class J>
JUTIL_CI auto match_call(callable<decltype(J{} + 0)> auto &&f, std::size_t,
                         J j) noexcept(noexcept(f(j + 0)))
    -> std::conditional_t<std::is_void_v<decltype(f(j + 0))>, decltype(j + 0), decltype(f(j + 0))>
{
    if constexpr (std::is_void_v<decltype(f(j + 0))>) {
        f(j + 0);
        return j + 0;
    } else return f(j + 0);
}

template <string... Ss, std::size_t... Is, bool Use256 = (((Ss.size() + 1) + ...) <= 32),
          class MaskTy = std::conditional_t<Use256, __mmask32, __mmask64>>
JUTIL_CI auto match(std::index_sequence<Is...> &&, const char *const s, auto &&f) noexcept((
    noexcept(match_call(f, idxty<Is>{}, meta::lift<static_cast<MaskTy>(match_idx<Ss...>[Is])>{})) &&
    ... && noexcept(match_call(f, idxty<sizeof...(Ss)>{}, meta::lift<MaskTy{0}>{}))))
    -> decltype(match_call(f, idxty<sizeof...(Ss)>{}, meta::lift<MaskTy{0}>{}))
{
    static constexpr auto &idx = match_idx<Ss...>;
    if consteval {
        const auto fn = [&]<std::size_t I, string S, string... Ss_>(auto fn_) {
            if (std::equal(S.begin(), S.end(), s)) {
                return match_call(f, idxty<I>{},
                                  meta::lift<static_cast<MaskTy>(idxty<idx[I]>{})>{});
            } else if constexpr (!sizeof...(Ss_)) {
                return match_call(f, idxty<I>{}, meta::lift<MaskTy{0}>{});
            } else return fn_.template operator()<I + 1, Ss_...>(fn_);
        };
        return fn.template operator()<0, Ss...>(fn);
    } else {
        const auto load = [&] noexcept [[gnu::always_inline]] {
            if constexpr (Use256) return _mm256_broadcastsi128_si256(_mm_loadu_epi8(s));
            else return _mm512_broadcast_i64x2(_mm_loadu_epi8(s));
        }();

        static constexpr auto msuf = [] {
            std::array<char, sizeof(load)> res;
            res.fill(-1);
            auto it = res.begin();
            (..., ([&]<std::size_t... Js, std::size_t N>(string<std::index_sequence<Js...>, N>) {
                 ((*it++ = (char)Js), ...);
                 ++it;
             }(Ss)));
            return std::bit_cast<decltype(load)>(res);
        }();
        JUTIL_dbge(puts("suf:"), print_bytes(load), print_bytes(msuf));
        const auto suf             = shuffle_epi8(load, msuf);

        static constexpr auto bcmp = [] {
            std::array<char, sizeof(load)> res;
            res.fill(0);
            auto it = res.begin();
            (..., ([&]<std::size_t... Js, std::size_t N>(string<std::index_sequence<Js...>, N> s_) {
                 ((*it++ = s_[Js]), ...);
                 ++it;
             }(Ss)));
            return std::bit_cast<decltype(load)>(res);
        }();
        JUTIL_dbge(puts("cmp:"), print_bytes(suf), print_bytes(bcmp));
        const MaskTy cmp             = cmpeq_epi8_mask(suf, bcmp);

        static constexpr MaskTy bsub = []<std::size_t... Js>(std::index_sequence<Js...>) {
            std::remove_const_t<decltype(cmp)> res = 0, i = 0;
            ((res |= ((decltype(res){1} << Ss.size()) - 1) << i, i += Ss.size() + 1), ...);
            return res;
        }(std::make_index_sequence<sizeof(cmp) * 8>{});
        JUTIL_dbge(puts("sub:"), print_bits(cmp), print_bits(bsub));
        const MaskTy sub = cmp - bsub;

        static constexpr MaskTy band =
            ~bsub & (~(decltype(bsub){0} >> (sizeof(bsub) * 8 - ((Ss.size() + 1) + ...))));
        JUTIL_dbge(puts("and:"), print_bits(sub), print_bits(band));
        const MaskTy and_ = sub & band;

        JUTIL_dbge(puts("res:"), print_bits(and_));
        JUTIL_dbge([&] {
            for (auto &&[i, y] : idx | sv::enumerate)
                printf("%zu. ", i + 1), print_bits(static_cast<MaskTy>(y));
        }());
        if constexpr (requires { match_call(f, -1uz, and_); }) {
            return match_call(f, -1uz, and_);
        } else {
            return meta::visit<meta::lifts<idx[Is]..., 0uz>>(
                and_, [&]<class T>(auto i) noexcept [[gnu::always_inline]] {
                    JUTIL_dbge(print_bits(T::value));
                    return match_call(f, i, meta::lift<T::value>{});
                });
        }
    }
}
} // namespace detail

/**
 * @brief Matches a string against a set of string literals and returns a unique identifier of the
 * matched string.
 *
 * @tparam Ss The string literals to match against.
 * @tparam F The function object to call on a successful match.
 * @param s A pointer to the beginning of the string to match.
 * @param _f The function object to call on a successful match.
 * @return A unique identifier for the matched string or return value of given function, `0` if no
 * match was found.
 */
template <string... Ss, class F>
    requires(((Ss.size() + 1) + ...) < 64)
JUTIL_CI auto match(const char *const s, F &&f) noexcept(noexcept(
    detail::match<Ss...>(std::make_index_sequence<sizeof...(Ss)>{}, s, static_cast<F &&>(f))))
    -> decltype(detail::match<Ss...>(std::make_index_sequence<sizeof...(Ss)>{}, s,
                                     static_cast<F &&>(f)))
{
    return detail::match<Ss...>(std::make_index_sequence<sizeof...(Ss)>{}, s, static_cast<F &&>(f));
}

// /**
//  * @brief Matches a string against a set of string literals and returns a unique identifier of
//  the
//  * matched string.
//  *
//  * @tparam Ss The string literals to match against.
//  * @param s A pointer to the beginning of the string to match.
//  * @return A unique identifier for the matched string, `0` if no match was found.
//  */
// template <string... Ss>
// [[nodiscard]] JUTIL_CI auto match(const char *const s) noexcept(
//     noexcept(match<Ss...>(s, [](std::integral auto) static noexcept {})))
//     -> decltype(match<Ss...>(s, [](std::integral auto) static noexcept {}))
// {
//     return match<Ss...>(s, [](std::integral auto) static noexcept {});
// }
} // namespace jutil
