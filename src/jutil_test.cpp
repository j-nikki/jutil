#include <jutil/alg.h>
#include <jutil/b64.h>
#include <jutil/bit.h>
#include <jutil/core.h>
#include <jutil/data.h>
#include <jutil/macro.h>
#include <jutil/match.h>
#include <jutil/meta.h>

// namespace sr = std::ranges;
// namespace sv = sr::views;
// namespace jm = jutil::meta;

// using namespace jutil;

// std::pair<const char *, const char *> take_request(char *buf)
// {
//     static constexpr std::string_view req = "PUT /";
//     memcpy(buf, req.data(), req.size());
//     return {buf, buf + req.size()};
// }

// extern "C" void _start(void)
// {
//     char buf[64];
//     const auto [f, l]  = take_request(buf);
//     const auto retcode = jutil::match<"GET ", "POST ", "PATCH ", "DELETE ", "PUT ">(f);
//     _Exit(retcode == 0 ? 1 : 0);
// }

JUTIL_NOINLINE std::pair<const char *, const char *> populate_inbuf(char *buf)
{
    static constexpr std::string_view req = "dXNlcm5hbWU6cGFzc3dvcmQ=";
    // static constexpr std::string_view req = "QUJDQUJD";
    memcpy(buf, req.data(), req.size());
    return {buf, buf + req.size()};
}

extern "C" void _start(void)
{
    char inbuf[64];
    std::array<char, 64> outbuf;
    const auto [f, l] = populate_inbuf(inbuf);
    auto out          = jutil::b64_decode(std::string_view{f, l}, outbuf);
    printf(".%.*s.\n", static_cast<int>(out - outbuf.begin()), outbuf.data());
    // const auto retcode = jutil::match<"GET ", "POST ", "PATCH ", "DELETE ", "PUT ">(f);
    _Exit(0);
}
