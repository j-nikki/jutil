#pragma once

#include "core.h"

namespace jutil
{
//
// curry
//
template <class, class, class...>
struct curry;
template <class F, std::size_t... Is, class... Ts>
struct [[nodiscard]] curry<F, std::index_sequence<Is...>, Ts...> {
    F f_;
    std::tuple<Ts...> xs_;

    template <class... Us>
        requires(is_callable<F, Ts && ..., Us && ...>)
    JUTIL_CI auto operator()(Us &&...ys) noexcept(noexcept(f_(std::get<Is>(std::move(xs_))...,
                                                              static_cast<Us &&>(ys)...)))
        -> decltype(f_(std::get<Is>(std::move(xs_))..., static_cast<Us &&>(ys)...))
    {
        return f_(std::get<Is>(std::move(xs_))..., static_cast<Us &&>(ys)...);
    }

    template <class... Us>
        requires(is_callable<F, const Ts &..., Us && ...>)
    JUTIL_CI auto operator()(Us &&...ys) const
        noexcept(noexcept(f_(std::get<Is>(xs_)..., static_cast<Us &&>(ys)...)))
            -> decltype(f_(std::get<Is>(xs_)..., static_cast<Us &&>(ys)...))
    {
        return f_(std::get<Is>(xs_)..., static_cast<Us &&>(ys)...);
    }

    template <class... Us>
        requires(!is_callable<F, Ts && ..., Us && ...>)
    JUTIL_CI curry<F, std::index_sequence_for<Ts..., Us...>, Ts..., Us...> operator()(
        Us &&...ys) noexcept(noexcept(curry<F, std::index_sequence_for<Ts..., Us...>, Ts..., Us...>{
        std::move(f_), {std::get<Is>(std::move(xs_))..., static_cast<Us &&>(ys)...}}))
    {
        return {std::move(f_), {std::get<Is>(std::move(xs_))..., static_cast<Us &&>(ys)...}};
    }

    template <class... Us>
        requires(!is_callable<F, const Ts &..., Us && ...>)
    JUTIL_CI curry<F, std::index_sequence_for<Ts..., Us...>, Ts..., Us...>
    operator()(Us &&...ys) const
        noexcept(noexcept(curry<F, std::index_sequence_for<Ts..., Us...>, Ts..., Us...>{
            std::move(f_), {std::get<Is>(xs_)..., static_cast<Us &&>(ys)...}}))
    {
        return {std::move(f_), {std::get<Is>(xs_)..., static_cast<Us &&>(ys)...}};
    }
};
template <class F>
curry(F &&) -> curry<F, std::index_sequence<>>;

//
// mirror_swap
//
constexpr inline curry mirror_swap{
    [](auto src, auto dst, auto a, auto b) noexcept(noexcept(
        sr::iter_swap(sr::next(src, sr::distance(dst, a)), sr::next(src, sr::distance(dst, b))))) {
        const auto aoff = sr::distance(dst, a);
        const auto boff = sr::distance(dst, b);
        sr::iter_swap(sr::next(src, aoff), sr::next(src, boff));
    }};
} // namespace jutil
