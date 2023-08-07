#include <jutil/b64.h>

namespace sr = std::ranges;

int main(int argc, char *argv[])
{
    using namespace jutil::literals;
    static constexpr auto str = "Many hands make light work."_s;
    char inbuf[64];
    sr::copy(str, inbuf);
    char buf[64];
    const auto res = jutil::b64_encode_pad(std::span{inbuf, str.size()}, buf);
    printf("-%.*s- (%d)\n", (int)(res - buf), buf, (int)(res - buf));
    const auto res2 = jutil::b64_decode_prune(std::span{buf, res}, inbuf);
    printf("-%.*s- (%d)\n", (int)(res2 - inbuf), inbuf, (int)(res2 - inbuf));
    return 0;
}
