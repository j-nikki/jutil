#pragma once

#include <algorithm>
#include <boost/mp11/algorithm.hpp>
#include <concepts>
#include <ranges>

#include "core.h"

namespace jutil
{
namespace mp = boost::mp11;
namespace sr = std::ranges;
//
// pair
//
template <class T, class U, bool Flip = (sizeof(U) > sizeof(T)),
          class Fst = std::conditional_t<Flip, U, T>, class Snd = std::conditional_t<Flip, T, U>>
class pair
{
    JUTIL_NO_UNIQUE_ADDRESS Fst fst;
    JUTIL_NO_UNIQUE_ADDRESS Snd snd;
    JUTIL_CI pair() = default;
    template <std::constructible_from<T> TCtor, std::constructible_from<U> UCtor>
        requires(!Flip)
    JUTIL_CI pair(TCtor &&t, UCtor &&u) noexcept(
        std::is_nothrow_constructible_v<T, TCtor> &&std::is_nothrow_constructible_v<U, UCtor>)
        : Fst{static_cast<TCtor &&>(t)}, Snd{static_cast<UCtor &&>(u)}
    {
    }
    template <std::constructible_from<T> TCtor, std::constructible_from<U> UCtor>
        requires(Flip)
    JUTIL_CI pair(TCtor &&t, UCtor &&u) noexcept(
        std::is_nothrow_constructible_v<T, TCtor> &&std::is_nothrow_constructible_v<U, UCtor>)
        : Fst{static_cast<UCtor &&>(u)}, Snd{static_cast<TCtor &&>(t)}
    {
    }
};

//
// tuple
//
namespace detail
{
template <std::size_t I, class T>
struct tuple_element;
#define JUTIL_tel(I, X)                                                                            \
    template <class T>                                                                             \
    struct tuple_element<I, T> {                                                                   \
        JUTIL_NO_UNIQUE_ADDRESS T X;                                                               \
                                                                                                   \
      protected:                                                                                   \
        JUTIL_CI tuple_element() noexcept                                                          \
            requires std::is_default_constructible_v<T>                                            \
        = default;                                                                                 \
        template <class U>                                                                         \
            requires(std::constructible_from<T, U>)                                                \
        JUTIL_CI tuple_element(U &&x_) noexcept(std::is_nothrow_constructible_v<T, U>)             \
            : X{static_cast<U &&>(x_)}                                                             \
        {                                                                                          \
        }                                                                                          \
        JUTIL_CI T &at(idxty<I>) noexcept { return X; }                                            \
        JUTIL_CI const T &at(idxty<I>) const noexcept { return X; }                                \
    }
// clang-format off
JUTIL_tel(0, a); JUTIL_tel(1, b); JUTIL_tel(2, c); JUTIL_tel(3, d); JUTIL_tel(4, e); JUTIL_tel(5, f); JUTIL_tel(6, g); JUTIL_tel(7, h); JUTIL_tel(8, i); JUTIL_tel(9, j); JUTIL_tel(10, k); JUTIL_tel(11, l); JUTIL_tel(12, m); JUTIL_tel(13, n); JUTIL_tel(14, o); JUTIL_tel(15, p); JUTIL_tel(16, q); JUTIL_tel(17, r); JUTIL_tel(18, s); JUTIL_tel(19, t); JUTIL_tel(20, u); JUTIL_tel(21, v); JUTIL_tel(22, w); JUTIL_tel(23, x); JUTIL_tel(24, y); JUTIL_tel(25, z);
// clang-format on

template <class...>
struct tuple_info;
template <std::size_t... Is, class... Ts>
struct tuple_info<std::index_sequence<Is...>, Ts...> {
    static constexpr inline auto mapidx = [] {
        const auto xs = if_<(sizeof...(Ts) > 0)>(
            [](auto... is) {
                std::array res{std::pair{sizeof(Ts), -to_signed(is)}...};
                sr::sort(res, sr::greater{});
                return res;
            },
            [] {
                return std::array<std::pair<std::size_t, std::make_signed_t<std::size_t>>, 0>{};
            })(Is...);
        std::array<std::size_t, sizeof...(Ts)> map, idx{};
        ((map[-xs[Is].second] = Is, idx[Is] = -xs[Is].second), ...);
        return std::pair{map, idx};
    }();
    static constexpr inline auto &map = mapidx.first;
    static constexpr inline auto &idx = mapidx.second;
    template <std::size_t I>
    using nthty = std::tuple_element_t<I, std::tuple<Ts...>>;
    template <std::size_t I>
    using elemty = tuple_element<I, nthty<I>>;
    using cmpcat = std::common_comparison_category_t<std::compare_three_way_result_t<Ts>...>;
    struct tuple : elemty<idx[Is]>... {
        //
        // constructors
        //
        JUTIL_CI tuple()
            requires(std::is_default_constructible_v<Ts> && ...)
        = default;
        template <class... TsCtor>
            requires(std::constructible_from<Ts, TsCtor &&> && ...)
        JUTIL_CI tuple(TsCtor &&...ts) noexcept((std::is_nothrow_constructible_v<Ts, TsCtor &&> &&
                                                 ...))
            : elemty<idx[Is]>{static_cast<TsCtor &&>(ts)}...
        {
        }

        //
        // member access
        //
        using elemty<Is>::at...;
        template <std::size_t I>
        JUTIL_CI nthty<I> &get() noexcept
        {
            return at(idxty<I>{});
        }
        template <std::size_t I>
        JUTIL_CI const nthty<I> &get() const noexcept
        {
            return at(idxty<I>{});
        }
        template <std::size_t I>
        JUTIL_CI nthty<I> &operator[](idxty<I>) noexcept
        {
            return get<I>();
        }
        template <std::size_t I>
        JUTIL_CI const nthty<I> &operator[](idxty<I>) const noexcept
        {
            return get<I>();
        }

        //
        // comparison
        //
        friend JUTIL_CI cmpcat operator<=>(const tuple &lhs, const tuple &rhs) noexcept
            requires(std::three_way_comparable<Ts> && ...)
        {
            const auto f = [&](auto f_, auto i) -> cmpcat {
                if constexpr (i == sizeof...(Ts) - 1) {
                    return lhs.at(i) <=> rhs.at(i);
                } else if (auto c = lhs.at(i) <=> rhs.at(i); c != 0) {
                    return c;
                } else {
                    return f_(f_, idxty<i + 1>{});
                }
            };
            return f(f, idxty<0>{});
        }

        //
        // swap
        //
        friend JUTIL_CI void swap(tuple &lhs, tuple &rhs) noexcept
            requires(std::is_nothrow_swappable_v<Ts> && ...)
        {
            using std::swap;
            ((swap(lhs.at(idxty<Is>{}), rhs.at(idxty<Is>{})), ...));
        }
    };
};
} // namespace detail
// using detail::tuple::tuple;
template <class... Ts>
struct tuple : detail::tuple_info<std::index_sequence_for<Ts...>, Ts...>::tuple {
    using detail::tuple_info<std::index_sequence_for<Ts...>, Ts...>::tuple::tuple;
};
template <class... Ts>
tuple(Ts &&...) -> tuple<Ts...>;
// static constexpr auto test = tuple{1, 2, 3};
// static_assert(std::is_same_v<decltype(test), const tuple<int, int, int>>);
template <std::size_t I, class... Ts>
[[nodiscard]] JUTIL_CI auto &get(jutil::tuple<Ts...> &xs) noexcept
{
    return xs.template get<I>();
}
template <std::size_t I, class... Ts>
[[nodiscard]] JUTIL_CI const auto &get(const jutil::tuple<Ts...> &xs) noexcept
{
    return xs.template get<I>();
}
template <std::size_t I, class... Ts>
[[nodiscard]] JUTIL_CI auto &&get(jutil::tuple<Ts...> &&xs) noexcept
{
    return std::move(xs).template get<I>();
}
} // namespace jutil
template <std::size_t I, class... Ts>
struct std::tuple_element<I, jutil::tuple<Ts...>> {
    using type = typename jutil::detail::tuple_info<Ts...>::nth<I>;
};
template <class... Ts>
struct std::tuple_size<jutil::tuple<Ts...>> : std::integral_constant<std::size_t, sizeof...(Ts)> {};
