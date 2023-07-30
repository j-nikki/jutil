#pragma once

#include <memory>

#include "core.h"
#include "data.h"

namespace jutil::detail
{
template <class T = uint8_t, class Al = std::allocator<T>, std::size_t N = 16>
class vec
{
    using altr = std::allocator_traits<Al>;
    static_assert(std::is_same_v<altr::value_type, T>);
    tuple<Al, typename altr::pointer, std::size_t> xs;

  public:
    JUTIL_CI vec(std::size_t n = N)
        requires(std::is_default_constructible_v<Al>)
    {
        xs.b = altr::allocate(xs.a, xs.c = CHECK(n, != 0, % N == 0));
    }
    template <std::constructible_from<Al> Arg>
    JUTIL_CI vec(Arg &&arg, std::size_t n = N) : xs{static_cast<Arg &&>(arg), nullptr, 0}
    {
        xs.b = altr::allocate(xs.a, xs.c = CHECK(n, != 0, % N == 0));
    }
    JUTIL_CI ~vec() { altr::deallocate(xs.a, xs.b, xs.c); }
    vec(const vec &)            = delete;
    vec &operator=(const vec &) = delete;
    vec(vec &&)                 = delete;
    vec &operator=(vec &&)      = delete;
    JUTIL_CI T &at(std::size_t i) noexcept { return xs.b[i]; }
    JUTIL_CI const T &at(std::size_t i) const noexcept { return xs.b[i]; }
    JUTIL_CI T &operator[](std::size_t i) noexcept { return at(i); }
    JUTIL_CI const T &operator[](std::size_t i) const noexcept { return at(i); }
    JUTIL_CI std::size_t size() const noexcept { return xs.c; }
    JUTIL_CI std::size_t capacity() const noexcept { return xs.c; }

    //
    // reallocation
    //
    JUTIL_CI void grow(std::size_t n)
        requires(std::is_nothrow_move_constructible_v<T>)
    {
        const auto p = xs.a.allocate(CHECK(n, > xs.c, % N == 0));
        [&] noexcept {
            std::uninitialized_move(xs.b, xs.b + xs.c, p);
            altr::deallocate(xs.a, xs.b, xs.c);
        }();
        xs.b = p;
        xs.c = n;
    }
    JUTIL_CI void shrink(std::size_t n)
        requires(std::is_nothrow_move_constructible_v<T>)
    {
        const auto p = xs.a.allocate(CHECK(n, < xs.c, % N == 0));
        [&] noexcept {
            std::uninitialized_move(xs.b, xs.b + xs.c, p);
            altr::deallocate(xs.a, xs.b, xs.c);
        }();
        xs.b = p;
        xs.c = n;
    }
};
} // namespace jutil::detail
