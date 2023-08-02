#pragma once

#include "alg.h"
#include "core.h"
#include "match.h"

namespace jutil
{
constexpr inline std::size_t help_narg = ~0uz;
template <string Short, string Long, std::size_t NArg, string Desc,
          auto F = []<class... Ts>(Ts &&...) {}>
struct command {
    static constexpr auto s = Short;
    static constexpr auto l = Long;
    static constexpr auto n = NArg;
    template <class... Ts>
    static JUTIL_CI int fn(auto &state, Ts &&...xs) noexcept
    {
        if constexpr (requires { F(state) = (static_cast<Ts &&>(xs), ...); }) {
            return F(state) = (static_cast<Ts &&>(xs), ...), 0;
        } else if constexpr (std::is_void_v<decltype(F(state, static_cast<Ts &&>(xs)...))>) {
            return F(state, static_cast<Ts &&>(xs)...), 0;
        } else return F(state, static_cast<Ts &&>(xs)...);
    }
};
using help_cmd = command<"h", "help", help_narg, "display this help and exit">;
namespace detail
{
template <string, class...>
struct argparse_t;
template <string Program, std::size_t... Is, string... Ss, string... Ls, std::size_t... Ns,
          string... Ds, auto... Fs>
struct argparse_t<Program, std::index_sequence<Is...>, command<Ss, Ls, Ns, Ds, Fs>...> {
    template <std::size_t I>
    using nth_cmd               = meta::nth<I, meta::list<command<Ss, Ls, Ns, Ds, Fs>...>>;
    static constexpr auto usage = [] {
        std::array<char, Program.size() + ((3 + Ss.size() + (Ns == 1 ? 6 : 1)) + ...) + 1> res{};
        auto it = sr::copy(Program, res.begin()).out;
        (..., (*it++ = ' ', *it++ = '[', *it++ = '-', it = sr::copy(Ss, it).out,
               it = sr::copy(if_<Ns == 1>(string{"<arg>]"}, string{"]"}), it).out));
        *it++ = '\n';
        return res;
    }();
    static constexpr auto msg_h = [] {
        constexpr auto ns = std::max({Ss.size()...}) + 1;
        constexpr auto nl = std::max({Ls.size()...}) + 2;
        std::array<char, usage.size() + (ns + nl + 5) * sizeof...(Ss) + (Ds.size() + ...) + 1>
            res{};
        res.fill(' ');
        auto it = sr::copy(usage, res.begin()).out;
        (..., (*it++ = '\t', *it++ = '-', sr::copy(Ss, it), it += ns, *it++ = '-', *it++ = '-',
               sr::copy(Ls, it), it += nl, it = sr::copy(Ds, it).out, *it++ = '\n'));
        *it++ = '\0';
        return res;
    }();
    static constexpr auto msg_e = [] {
        std::array<char, usage.size() + 8> res;
        *sr::copy(usage, sr::copy(string{"usage: "}, res.begin()).out).out = '\0';
        return res;
    }();
    template <class T>
    struct parse_res {
        T state;
        int argi;
    };
    static constexpr std::tuple ss{Ss..., string{prepend<'-'>(Ls)}...};
    template <callable<> F, class R1 = std::decay_t<decltype(std::declval<F &>()())>,
              callable_r<int, R1 &&, std::span<const char *const>> G>
    static JUTIL_CI int operator()(const int argc_, const char *const argv_[], F &&f_, G &&g_ = {})
    {
#define JUTIL_ap_p(Capture, Static, F, G)                                                          \
    return [RMPARENS Capture](const int argc, const char *const argv[])                            \
        RMPARENS Static noexcept(noexcept(G(F(), std::span<const char *const>{})))                 \
            __attribute__((optimize("-Os")))                                                       \
                ->int                                                                              \
    {                                                                                              \
        int ret    = 0, i;                                                                         \
        auto state = F();                                                                          \
        for (i = 1; i < argc; ++i) {                                                               \
            const char *arg = argv[i];                                                             \
            if (*arg != '-') break;                                                                \
            jutil::match<std::get<Is / 2 + Is % 2 * sizeof...(Ss)>(ss)...>(                        \
                arg + 1, [&]<std::size_t I>(idxty<I>, auto) noexcept {                             \
                    if constexpr (I == sizeof...(Is)) fputs(msg_e.data(), stderr), ret = 1;        \
                    else {                                                                         \
                        JUTIL_dbge(printf("match arg %zu\n", I / 2));                              \
                        static constexpr auto s  = I % 2 == 0;                                     \
                        using cmd                = nth_cmd<I / 2>;                                 \
                        static constexpr auto &m = if_<s>(cmd::s, cmd::l);                         \
                        const auto f = [&](auto... xs) { return cmd::fn(state, xs...); };          \
                        if constexpr (cmd::n == help_narg) fputs(msg_h.data(), stderr), ret = 1;   \
                        else if constexpr (cmd::n) {                                               \
                            if constexpr (s) ret = f(arg + 1 + m.size());                          \
                            else if (arg[m.size()] == '=') ret = f(arg + 1 + m.size() + 1);        \
                            else ret = 1;                                                          \
                        } else if (arg[m.size()]) ret = 1;                                         \
                        else ret = f();                                                            \
                    }                                                                              \
                });                                                                                \
            if (ret) return ret;                                                                   \
        }                                                                                          \
        return G(std::move(state), std::span<const char *const>{&argv[i], &argv[argc - i]});       \
    }                                                                                              \
    (argc_, argv_)
        static constexpr auto fidc = std::is_default_constructible_v<F>;
        static constexpr auto gidc = std::is_default_constructible_v<G>;
        if constexpr (fidc && gidc) JUTIL_ap_p((), (static), F{}, G{});
        else JUTIL_ap_p((&), (), f_, g_);
    }
};
} // namespace detail
template <string Prog, class... Cmds>
constexpr inline detail::argparse_t<Prog, std::make_index_sequence<sizeof...(Cmds) * 2>, Cmds...>
    argparse;
} // namespace jutil
