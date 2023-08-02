#include <jutil/argparse.h>

#include <jutil/lmacro.inl>

int main(int argc, char *argv[])
{
    using namespace jutil;
    struct args_t {
        const char *host = "0.0.0.0";
        const char *port = "8080";
        const char *root = ".";
    };
    auto ap =
        argparse<"jutil", help_cmd,                                                               //
                 command<"H", "host", 1, "address to host on, default 0.0.0.0", L(x.host)>,       //
                 command<"p", "port", 1, "port to host on, default 8080", L(x.port)>,             //
                 command<"r", "root", 1, "root to dir to serve files from, default .", L(x.root)> //
                 >;
    return ap(
        argc, argv, [] { return args_t{}; },
        [](auto args, auto) {
            printf("starting server at https://%s:%s with root %s\n", args.host, args.port,
                   args.root);
            return 0;
        });
}

#include <jutil/lmacro.inl>
