#include "jutil/alg.h"
#include "jutil/b64.h"
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
        if (i && i % 8 == 0) fputc('\'', f);
        fprintf(f, "%d", (int)!!(p[j / 8] & (1 << (j % 8))));
    }
    fprintf(f, "\n");
}

template <char... Cs>
constexpr auto vec =
    std::bit_cast<__m512i>(std::array<uint64_t, 8>{(Cs * 0x1010101'01010101l)..., 0l});

template <char... Cs>
constexpr auto setr_epi8 = std::bit_cast<__m512i>(std::array<char, 64>{Cs...});

void b64_decode(const char *f, const char *const l, char *dit) noexcept
{
    for (; f != l; f += 8, dit += 6) {
        JUTIL_dbge(printf("----\n"));
        const auto xs = _mm512_set1_epi64(jutil::loadu<int64_t>(f));
        JUTIL_dbge(puts("xs: "), print_bytes(xs));

        const auto sub = _mm512_sub_epi8(xs, vec<'A', 'a', '0', '+', '/', '-', '_'>);
        JUTIL_dbge(puts("sub: "), print_bytes(sub));
        const auto lt = _mm512_cmplt_epu8_mask(sub, vec<26, 26, 10, 1, 1, 1, 1>);
        JUTIL_dbge(puts("lt: "), print_bits(lt));

        const auto add = _mm512_maskz_add_epi8(lt, sub, vec<0, 26, 52, 62, 63, 62, 63>);
        JUTIL_dbge(puts("add: "), print_bytes(add));
        const auto or_ = _mm512_reduce_or_epi64(add);
        JUTIL_dbge(puts("or: "), print_bits(or_));

        const auto bc = _mm512_set1_epi64(or_);
        const auto bs = _mm512_bitshuffle_epi64_mask(
            bc, setr_epi8<014, 015, 000, 001, 002, 003, 004, 005, //
                          022, 023, 024, 025, 010, 011, 012, 013, //
                          030, 031, 032, 033, 034, 035, 020, 021, //
                          054, 055, 040, 041, 042, 043, 044, 045, //
                          062, 063, 064, 065, 050, 051, 052, 053, //
                          070, 071, 072, 073, 074, 075, 060, 061, //
                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>);
        JUTIL_dbge(puts("bs: "), print_bits(bs));
        storeu(dit, bs);
    }
}
} // namespace jutil::detail
