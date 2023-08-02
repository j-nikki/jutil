#pragma once

#include "core.h"
#include "macro.h"

namespace jutil::meta
{
template <auto, class>
struct filter;
template <auto P, template <class...> class T, class... Us>
struct filter<P, T<Us...>> {
    using type = decltype((std::tuple_cat(
        std::conditional_t<P.template operator()<Us>(), std::tuple<Us>, std::tuple<>>{}...)));
};
template <auto P, class T>
using filter_t = filter<P, T>::type;

template <class... Ts>
struct with_t {
    template <class... Us>
    static JUTIL_CI auto operator()(auto &&f, Us &&...xs) noexcept(
        noexcept(f.template operator()<Ts...>(static_cast<Us &&>(xs)...)))
        -> decltype(f.template operator()<Ts...>(static_cast<Us &&>(xs)...))
    {
        return f.template operator()<Ts...>(static_cast<Us &&>(xs)...);
    }
};
template <class... Ts>
constexpr inline with_t<Ts...> with;

template <class>
struct apply_t;
template <template <class...> class T, class... Us>
struct apply_t<T<Us...>> {
    template <class... Vs>
    static JUTIL_CI auto operator()(auto &&f, Vs &&...args) noexcept(
        noexcept(f.template operator()<Us...>(static_cast<Vs &&>(args)...)))
        -> decltype(f.template operator()<Us...>(static_cast<Vs &&>(args)...))
    {
        return f.template operator()<Us...>(static_cast<Vs &&>(args)...);
    }
};
template <class T>
constexpr inline apply_t<T> apply;

template <template <class...> class, class>
struct rename_t;
template <template <class...> class V, template <class...> class T, class... Us>
struct rename_t<V, T<Us...>> {
    using type = V<Us...>;
};
template <template <class...> class V, class T>
using rename = rename_t<V, T>::type;

template <class, class>
struct any_t;
template <std::size_t... Is, template <class...> class T, class... Us>
struct any_t<std::index_sequence<Is...>, T<Us...>> {
    template <class... Vs>
    static JUTIL_CI auto operator()(auto &&f, Vs &&...args) noexcept(
        noexcept((f.template operator()<Us>(static_cast<Vs &&>(args)...) || ... || false)))
        -> std::common_type_t<decltype(f.template operator()<Us>(static_cast<Vs &&>(args)...))...>
        requires(requires {
            (f.template operator()<Us>(static_cast<Vs &&>(args)...) || ... || false);
        })
    {
        std::common_type_t<decltype(f.template operator()<Us>(static_cast<Vs &&>(args)...))...>
            res = {};
        ((res = f.template operator()<Us>(static_cast<Vs &&>(args)...)) || ...);
        return res;
    }
    template <class... Vs>
    [[nodiscard]] static JUTIL_CI auto operator()(auto &&f, Vs &&...args) noexcept(noexcept(
        (f.template operator()<Us>(idxty<Is>{}, static_cast<Vs &&>(args)...) || ... || false)))
        -> std::common_type_t<decltype(f.template operator()<Us>(idxty<Is>{},
                                                                 static_cast<Vs &&>(args)...))...>
        requires(requires {
            (f.template operator()<Us>(idxty<Is>{}, static_cast<Vs &&>(args)...) || ... || false);
        })
    {
        std::common_type_t<decltype(f.template operator()<Us>(idxty<Is>{},
                                                              static_cast<Vs &&>(args)...))...>
            res = {};
        ((res = f.template operator()<Us>(idxty<Is>{}, static_cast<Vs &&>(args)...)) || ...);
        return res;
    }
};
template <class T>
constexpr inline any_t<rename<std::index_sequence_for, T>, T> any;

template <auto X>
struct lift : std::integral_constant<decltype(X), X> {};

template <class... Ts>
struct list {};

template <auto... Xs>
using lifts = list<lift<Xs>...>;

template <std::size_t I, class T>
using nth = std::tuple_element_t<I, rename<std::tuple, T>>;

template <class, class>
struct map_t;
template <class F, template <class...> class T, class... Us>
struct map_t<F, T<Us...>> {
    using type = T<decltype(F{}.template operator()<Us>())...>;
};
template <class F, class T>
using map = map_t<F, T>::type;

template <class, class>
struct visit_t;
template <std::size_t... Is, template <class...> class T, class... Us>
struct visit_t<std::index_sequence<Is...>, T<Us...>> {
    static constexpr auto max = std::max({Us::value...});
    template <std::size_t I>
    using nth_ = nth<std::min(I, sizeof...(Us)), list<Us..., lift<max + 1>>>;
    static JUTIL_CI auto
    operator()(auto I, auto &&f) noexcept((noexcept(f.template operator()<Us>(idxty<Is>{})) && ...))
        -> std::common_type_t<decltype(f.template operator()<Us>(idxty<Is>{}))...>
    {
        switch (I) {
#define JUTIL_mvc(X)                                                                               \
    case nth_<X>::value + (X < sizeof...(Us) ? 0 : X):                                             \
        if constexpr (X >= sizeof...(Us))                                                          \
            JUTIL_UNREACHABLE();                                                                   \
        else return f.template operator()<nth_<X>>(idxty<X>{});
            // clang-format off
            JUTIL_mvc(0uz)JUTIL_mvc(1uz)JUTIL_mvc(2uz)JUTIL_mvc(3uz)JUTIL_mvc(4uz)JUTIL_mvc(5uz)JUTIL_mvc(6uz)JUTIL_mvc(7uz)JUTIL_mvc(8uz)JUTIL_mvc(9uz)JUTIL_mvc(10uz)JUTIL_mvc(11uz)JUTIL_mvc(12uz)JUTIL_mvc(13uz)JUTIL_mvc(14uz)JUTIL_mvc(15uz)JUTIL_mvc(16uz)JUTIL_mvc(17uz)JUTIL_mvc(18uz)JUTIL_mvc(19uz)JUTIL_mvc(20uz)JUTIL_mvc(21uz)JUTIL_mvc(22uz)JUTIL_mvc(23uz)JUTIL_mvc(24uz)JUTIL_mvc(25uz)JUTIL_mvc(26uz)JUTIL_mvc(27uz)JUTIL_mvc(28uz)JUTIL_mvc(29uz)JUTIL_mvc(30uz)JUTIL_mvc(31uz)
                // clang-format on
                JUTIL_NO_DEFAULT();
        }
    }
};
template <class T>
constexpr inline visit_t<rename<std::index_sequence_for, T>, T> visit;
} // namespace jutil::meta
