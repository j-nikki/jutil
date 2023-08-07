#pragma once

#define PARENS               _PARENS
#define _PARENS              ()
#define RMPARENS(X)          _RMPARENS(X)
#define _RMPARENS(X)         X
#define ID(...)              __VA_ARGS__

#define EXP(...)             _EXP1(_EXP1(_EXP1(_EXP1(__VA_ARGS__))))
#define _EXP1(...)           _EXP2(_EXP2(_EXP2(_EXP2(__VA_ARGS__))))
#define _EXP2(...)           __VA_ARGS__

#define LAST(X, ...)         EXP(_LAST_EXP_##__VA_OPT__(AGAIN) PARENS(X __VA_OPT__(, ) __VA_ARGS__))
#define _LAST_EXP(X, Y, ...) _LAST_EXP_##__VA_OPT__(AGAIN) PARENS(Y __VA_OPT__(, ) __VA_ARGS__)
#define _LAST_EXP_AGAIN()    _LAST_EXP
#define _LAST_EXP_()         _LAST_EXP_DONE
#define _LAST_EXP_DONE(X)    X

#define RMLAST(X, ...)       EXP(_RMLAST_EXP_##__VA_OPT__(AGAIN) PARENS(X __VA_OPT__(, ) __VA_ARGS__))
#define _RMLAST_EXP(X, Y, ...)                                                                     \
    X __VA_OPT__(, ) _RMLAST_EXP_##__VA_OPT__(AGAIN) PARENS(Y __VA_OPT__(, ) __VA_ARGS__)
#define _RMLAST_EXP_AGAIN() _RMLAST_EXP
#define _RMLAST_EXP_()      _RMLAST_EXP_DONE
#define _RMLAST_EXP_DONE(X)

#define FOR_EACH(M, ...) EXP(_FOR_EACH_EXP_##__VA_OPT__(AGAIN) PARENS(M __VA_OPT__(, ) __VA_ARGS__))
#define _FOR_EACH_EXP(M, X, ...)                                                                   \
    M(X __VA_OPT__(, __VA_ARGS__))                                                                 \
    _FOR_EACH_EXP_##__VA_OPT__(AGAIN) PARENS(M __VA_OPT__(, ) __VA_ARGS__)
#define _FOR_EACH_EXP_AGAIN() _FOR_EACH_EXP
#define _FOR_EACH_EXP_()      _FOR_EACH_EXP_DONE
#define _FOR_EACH_EXP_DONE(M)

#define STR(X)         _STR_EXP(X)
#define _STR_EXP(X)    #X

#define FST(X, ...)    X
#define SND(X, Y, ...) Y
#define TAIL(X, ...)   __VA_ARGS__
#define WCOMMA(...)    __VA_OPT__(, __VA_ARGS__)

#define CAT(X, Y)      _CAT(X, Y)
#define _CAT(X, Y)     X##Y
