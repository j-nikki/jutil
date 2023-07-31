#pragma once

#include "alg.h"

namespace jutil
{
namespace detail
{
void b64_decode(const std::size_t n, const char *it, char *dit) noexcept;
} // namespace detail
template <sized_input_range I, constant_sized_output_range<char> O>
JUTIL_INLINE sr::iterator_t<O> b64_decode(I &&i, O &&o) noexcept
{
    CHECK(csr_sz<O> >= (sr::size(i) + 7) / 8 * 8);
    detail::b64_decode((sr::size(i) + 7) / 8 * 8, &*sr::begin(i), &*sr::begin(o));
    return sr::begin(o) + (sr::size(i) / 4 * 3);
}
} // namespace jutil
