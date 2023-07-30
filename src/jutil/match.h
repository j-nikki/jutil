#pragma once

#include "data.h"
#include "meta.h"

namespace jutil
{
struct string_view {
    const char *p;
    std::size_t n;
};

namespace detail
{
template <auto Fmt>
JUTIL_CI auto match(const char *it, const char *const l, auto &&...xs) noexcept
{
#define MRET(E, T)                                                                                 \
    do {                                                                                           \
        if (E) {                                                                                   \
            return T;                                                                              \
        } else {                                                                                   \
            auto res    = decltype(T){};                                                           \
            get<0>(res) = nullptr;                                                                 \
            return res;                                                                            \
        }                                                                                          \
    } while (0)
    if constexpr (Fmt.empty()) {
        return jutil::tuple{std::move(it), std::move(xs)...};
    } else if constexpr (Fmt[0] == '%') {
        if constexpr (static constexpr std::size_t i = Fmt[1] - '0'; i < 10) {
            return [&]<std::size_t... Is> [[gnu::always_inline]] (
                       std::index_sequence<Is...>) noexcept {
                MRET(it + i <= l,
                     match<drop<2>(Fmt)>(it + i, l, std::move(xs)..., std::array{it[Is]...}));
            }(std::make_index_sequence<i>{});
        } else if constexpr (Fmt[1] == 's') {
            auto it2 = it;
            *l       = Fmt[2];
            for (; *it2 != Fmt[2]; ++it2)
                ;
            MRET(it2 != l,
                 match<drop<2>(Fmt)>(it2, l, std::move(xs)..., string_view{it, it2 - it}));
        }
#if JUTIL_HOSTED
#define MCHCNV(X, ...)                                                                             \
    else if constexpr (Fmt[1] == FST X)                                                            \
    {                                                                                              \
        SND X val;                                                                                 \
        auto [it2, ec] = std::from_chars(it, l, val);                                              \
        MRET(ec == std::errc{}, match<drop<2>(Fmt)>(it2, l, std::move(xs)..., val));               \
    }
        FOR_EACH(MCHCNV, ('d', int), ('u', unsigned int), ('f', float))
#endif
        else
        {
            static_assert(false, "bad format");
        }
    } else {
        static constexpr auto f2          = sr::find(Fmt, '%');
        static constexpr std::size_t nstr = f2 - sr::begin(Fmt);
        static constexpr auto str         = take<nstr>(Fmt);
        MRET(std::equal(it, it + nstr, str.data()),
             match<drop<nstr>(Fmt)>(it + nstr, l, std::move(xs)...));
    }
}
} // namespace detail
template <string S>
[[nodiscard]] JUTIL_CI auto match(const char *f, const char *l) noexcept
{
    return detail::match<S>(f, l);
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
    } else
        return f(I{}, J{});
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
    } else
        return f(J{});
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
    } else
        return f(i, j + 0);
}

template <class J>
JUTIL_CI auto match_call(callable<decltype(J{} + 0)> auto &&f, std::size_t,
                         J j) noexcept(noexcept(f(j + 0)))
    -> std::conditional_t<std::is_void_v<decltype(f(j + 0))>, decltype(j + 0), decltype(f(j + 0))>
{
    if constexpr (std::is_void_v<decltype(f(j + 0))>) {
        f(j + 0);
        return j + 0;
    } else
        return f(j + 0);
}

template <class F, string... Ss, std::size_t... Is, bool Use256 = (((Ss.size() + 1) + ...) <= 32),
          class MaskTy = std::conditional_t<Use256, __mmask32, __mmask64>>
JUTIL_CI auto match(std::index_sequence<Is...> &&, const char *const s) noexcept(
    (noexcept(match_call(F{}, idxty<Is>{},
                         meta::lift<static_cast<MaskTy>(match_idx<Ss...>[Is])>{})) &&
     ... && match_call(F{}, idxty<sizeof...(Ss)>{}, meta::lift<MaskTy{0}>{})))
    -> decltype(match_call(F{}, idxty<sizeof...(Is)>{}, meta::lift<MaskTy{0}>{}))
{
    static constexpr auto &idx = match_idx<Ss...>;
    if consteval {
        const auto fn = [&]<std::size_t I, string S, string... Ss_>(auto fn_) {
            if (std::equal(S.begin(), S.end(), s)) {
                return match_call(F{}, idxty<I>{},
                                  meta::lift<static_cast<MaskTy>(idxty<idx[I]>{})>{});
            } else if constexpr (!sizeof...(Ss_)) {
                return match_call(F{}, idxty<I>{}, meta::lift<MaskTy{0}>{});
            } else
                return fn_.template operator()<I + 1, Ss_...>(fn_);
        };
        return fn.template operator()<0, Ss...>(fn);
    } else {
        const auto load = [&] noexcept [[gnu::always_inline]] {
            if constexpr (Use256)
                return _mm256_broadcastsi128_si256(_mm_loadu_epi8(s));
            else
                return _mm512_broadcast_i64x2(_mm_loadu_epi8(s));
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
        const MaskTy sub             = cmp - bsub;

        static constexpr MaskTy band = ~bsub & ((decltype(bsub){1} << ((Ss.size() + 1) + ...)) - 1);
        JUTIL_dbge(puts("and:"), print_bits(sub), print_bits(band));
        const MaskTy and_ = sub & band;

        JUTIL_dbge(puts("res:"), print_bits(and_));
        JUTIL_dbge([&] {
            for (auto &&[i, y] : idx | sv::enumerate)
                printf("%zu. ", i + 1), print_bits(static_cast<MaskTy>(y));
        }());

        if constexpr (std::is_invocable_v<F &&, MaskTy>) {
            return match_call(F{}, -1uz, and_);
        } else {
            return meta::visit<meta::lifts<0uz, idx[Is]...>>(
                and_, []<class T>(auto i) static noexcept [[gnu::always_inline]] {
                    JUTIL_dbge(print_bits(T::value));
                    return match_call(F{}, i, meta::lift<T::value>{});
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
JUTIL_CI auto match(const char *const s, [[maybe_unused]] F &&_f) noexcept(
    noexcept(detail::match<F, Ss...>(std::make_index_sequence<sizeof...(Ss)>{}, s)))
    -> decltype(detail::match<F, Ss...>(std::make_index_sequence<sizeof...(Ss)>{}, s))
{
    return detail::match<F, Ss...>(std::make_index_sequence<sizeof...(Ss)>{}, s);
}

/**
 * @brief Matches a string against a set of string literals and returns a unique identifier of the
 * matched string.
 *
 * @tparam Ss The string literals to match against.
 * @param s A pointer to the beginning of the string to match.
 * @return A unique identifier for the matched string, `0` if no match was found.
 */
template <string... Ss>
[[nodiscard]] JUTIL_CI auto match(const char *const s) noexcept(
    noexcept(match<Ss...>(s, [](std::integral auto) static noexcept {})))
    -> decltype(match<Ss...>(s, [](std::integral auto) static noexcept {}))
{
    return match<Ss...>(s, [](std::integral auto) static noexcept {});
}
} // namespace jutil
