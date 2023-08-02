#pragma once

#include "alg.h"

namespace jutil
{
namespace detail
{
void b64_decode(const char *f, const char *const l, char *dit) noexcept;
} // namespace detail
template <sized_input_range I, sized_output_range<char> O>
JUTIL_INLINE sr::iterator_t<O> b64_decode(I &&i, O &&o) noexcept
{
    const auto n = ((sr::size(i) + 7) / 8 * 8);
    CHECK(sr::size(o) >= n + 2); // we write 8B at a time (where last 2B is thrash)
    const auto it = &*sr::begin(i);
    detail::b64_decode(it, it + n, &*sr::begin(o));
    return sr::begin(o) + (sr::size(i) / 4 * 3);
}
} // namespace jutil
