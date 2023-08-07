#pragma once

#include "alg.h"

namespace jutil
{
namespace detail
{
void b64_decode(const char *f, const char *const l, char *dit) noexcept;
void b64_encode(const char *f, const char *const l, char *dit) noexcept;
} // namespace detail

//
// decode
//
template <sr::contiguous_range I, contiguous_output_range<char> O>
JUTIL_INLINE sr::iterator_t<O> b64_decode(I &&i, O &&o) noexcept
{
    const auto n = ((sr::size(i) + 7) / 8);
    CHECK(sr::size(i) % 4, == 0);
    CHECK(sr::size(o), >= n * 6 + 2); // we write 8B at a time (where last 2B is thrash)
    const auto it = sr::data(i);
    detail::b64_decode(it, it + n * 8, sr::data(o));
    return sr::data(o) + sr::size(i) / 4 * 3;
}
template <sr::contiguous_range I, contiguous_output_range<char> O>
JUTIL_INLINE sr::iterator_t<O> b64_decode_prune(I &&i, O &&o) noexcept
{
    const auto pad = !sr::size(i)                          ? 0
                     : sr::data(i)[sr::size(i) - 2] == '=' ? 2
                     : sr::data(i)[sr::size(i) - 1] == '=' ? 1
                                                           : 0;
    return b64_decode(static_cast<I &&>(i), static_cast<O &&>(o)) - pad;
}

//
// encode
//
template <sr::contiguous_range I, contiguous_output_range<char> O>
JUTIL_INLINE sr::iterator_t<O> b64_encode_unchecked(I &&i, O &&o) noexcept
{
    const auto n = ((sr::size(i) + 5) / 6);
    CHECK(sr::size(o) >= n * 8);
    const auto it = sr::data(i);
    detail::b64_encode(it, it + n * 6, sr::data(o));
    return sr::data(o) + sr::size(i) / 3 * 4;
}
template <sr::contiguous_range I, contiguous_output_range<char> O>
JUTIL_INLINE sr::iterator_t<O> b64_encode(I &&i, O &&o) noexcept
{
    CHECK(sr::size(i) % 3, == 0);
    return b64_encode_unchecked(static_cast<I &&>(i), static_cast<O &&>(o));
}
template <contiguous_output_range<char> I, contiguous_output_range<char> O>
JUTIL_INLINE sr::iterator_t<O> b64_encode_pad(I &&i, O &&o) noexcept
{
    const auto mod = sr::size(i) % 3;
    switch (mod) {
    case 1: sr::data(i)[sr::size(i) + 1] = 0; [[fallthrough]];
    case 2: sr::data(i)[sr::size(i) + 0] = 0; [[fallthrough]];
    case 0: break; JUTIL_NO_DEFAULT();
    }
    const auto res = sr::data(o) + (sr::size(i) + 2) / 3 * 4;
    b64_encode_unchecked(static_cast<I &&>(i), static_cast<O &&>(o));
    switch (mod) {
    case 1: res[-2] = '='; [[fallthrough]];
    case 2: res[-1] = '='; [[fallthrough]];
    case 0: break; JUTIL_NO_DEFAULT();
    }
    return res;
}
} // namespace jutil
