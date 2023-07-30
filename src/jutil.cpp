#include "jutil/alg.h"
#include "jutil/bit.h"
#include "jutil/core.h"

namespace jutil::detail
{
void fprint_bytes(FILE *const f, const char *const p, const std::size_t n) noexcept
{
    for (auto &&[i, c] : std::span{p, n} | sv::reverse | sv::enumerate) {
        if (i && i % 4 == 0) fputc('\'', f);
        fprintf(f, "%02x", (unsigned char)c);
    }
    fprintf(f, "\n");
}
void fprint_bits(FILE *const f, const char *const p, const std::size_t n) noexcept
{
    for (auto &&[i, j] : sv::iota(0uz, n * 8) | sv::reverse | sv::enumerate) {
        if (i && i % 4 == 0) fputc('\'', f);
        fprintf(f, "%d", (int)!!(p[j / 8] & (1 << (j % 8))));
    }
    fprintf(f, "\n");
}
} // namespace jutil::detail
