#pramga once

#include <boost/mp11/algorithm.hpp>
#include <range/v3/view/zip.hpp>
#include <span>

#include "core.h"

namespace jutil
{
namespace r3 = ranges::v3;
namespace rv = r3::view;
namespace ra = r3::actions;
namespace bm = boost::mp11;

namespace detail
{
template <class...>
struct soa_ty;
template <std::size_t... Is, class... Ts>
class soa_ty<std::index_sequence<Is...>, Ts...>
{
    using offs_type            = std::array<std::size_t, sizeof...(Ts)>;
    using this_type            = soa_ty<Ts...>;

    static constexpr auto offs = [] {
        std::array xs{std::pair{sizeof(Ts), Is}...};
        sr::sort(xs, sr::greater{});
        std::size_t i = 0;
        offs_type res{};
        ((res[xs[Is].second] = i, i += xs[Is].first), ...);
        return res;
    }();

  public:
    static JUTIL_CI std::size_t at(const std::size_t i) noexcept { return offs[i]; }

    static JUTIL_CI std::size_t size(const std::size_t nxs) noexcept
    {
        return (sizeof(Ts) + ...) * nxs;
    }

    template <auto... Xs>
    JUTIL_INLINE auto operator()(iseqty<Xs...>, std::byte *const p, const std::size_t cap,
                                 const std::size_t sz) const noexcept
    {
        if constexpr (sizeof...(Xs) == 1)
            return std::span{
                reinterpret_cast<bm::mp_at_c<this_type, (Xs, ...)> *>(p + cap * at(Xs...)), sz};
        else
            return r3::zip_view{
                std::span{reinterpret_cast<bm::mp_at_c<this_type, Xs> *>(p + cap * at(Xs)), sz}...};
    }
    template <auto... Xs>
    JUTIL_INLINE auto operator()(iseqty<Xs...> idx, std::byte *const p,
                                 const std::size_t cap) const noexcept
    {
        return operator()(idx, p, cap, cap);
    }

    JUTIL_INLINE auto operator()(std::byte *const p, const std::size_t cap,
                                 const std::size_t sz) const noexcept
    {
        return r3::zip_view{at<Is>(p, cap, sz)...};
    }
    JUTIL_INLINE auto operator()(std::byte *const p, const std::size_t cap) const noexcept
    {
        return operator()(p, cap, cap);
    }
};
} // namespace detail
template <class... Ts>
constexpr inline detail::soa_ty<std::index_sequence_for<Ts...>, Ts...> soa;
} // namespace jutil
