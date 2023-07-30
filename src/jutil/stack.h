#pragma once

#include "core.h"

namespace jutil
{
namespace detail
{
template <class T, T N, T Init>
struct stack {
    using value_type = T;
    JUTIL_CI std::size_t size() const noexcept
        requires(requires(T x) { bsr(x); })
    {
        return x_ ? bsr(x_) / N : 0;
    }
    [[nodiscard]] constexpr T peek() const noexcept { return x_ & ((1 << N) - 1); }
    JUTIL_CI void add(const T x) noexcept { CHECK(peek() + x, < (1 << N)), x_ += x; }
    JUTIL_CI void sub(const T x) noexcept { CHECK(peek() - x, >= 0), x_ += x; }
    JUTIL_CI void push(const T x) noexcept { grow(), add(x); }
    JUTIL_CI void grow() noexcept { x_ <<= N; }
    JUTIL_CI void shrink() noexcept { x_ >>= N; }
    [[nodiscard]] std::strong_ordering operator<=>(const stack &rhs) const = default;

  private:
    T x_ = Init;
};
template <class S>
struct iterator {
    using value_type      = S::value_type;
    using difference_type = std::ptrdiff_t;
    S s;
    struct end {};
    JUTIL_CI bool operator==(end) const noexcept { return s == S{}; }
    JUTIL_CI bool operator!=(end) const noexcept { return s != S{}; }
    JUTIL_CI bool operator==(iterator) const noexcept { return false; }
    JUTIL_CI bool operator!=(iterator) const noexcept { return false; }
    iterator &operator++() noexcept { return s.shrink(), *this; }
    iterator &operator++(int) noexcept
    {
        const auto res = *this;
        return ++*this, res;
    }
    auto operator*() const noexcept -> decltype(s.peek()) { return s.peek(); }
    auto operator*() noexcept -> decltype(s.peek()) { return s.peek(); }
};
} // namespace detail
template <class T = std::size_t, T N = 8, T Init = 0>
struct stack : detail::stack<T, N, Init> {
    using iterator = detail::iterator<detail::stack<T, N, Init>>;
    JUTIL_CI iterator begin() const noexcept { return {*this}; }
    JUTIL_CI iterator::end end() const noexcept { return {}; }
    [[nodiscard]] std::strong_ordering operator<=>(const stack &rhs) const = default;

  private:
    T x_ = Init;
};
} // namespace jutil
