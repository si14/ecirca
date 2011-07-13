/* Copyright (C) 2011 by Alexander Pantyukhov <alwx.main@gmail.com>
                         Dmitry Groshev       <lambdadmitry@gmail.com>
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
   THE SOFTWARE. */


#include "erl_nif.h"
#include <inttypes.h>

#define ELEM_TYPE       uint32_t
#define SIZE_TYPE       uint32_t
#define ERL_MAKE_ELEM   enif_make_uint
#define ERL_GET_ELEM    enif_get_uint
#define ERL_GET_SIZE    enif_get_uint
#define MAX_SLICE       10
#define EMPTY_VAL       -1
#define MAX_SIZE        100

/* data structure */
typedef struct {
    SIZE_TYPE           begin;
    ELEM_TYPE           *circa;
    SIZE_TYPE           size;
    unsigned short int  filled;
} circactx;

ErlNifResourceType* circa_type;

/* get array index with respect to array bounds */
SIZE_TYPE getIndex(circactx* ctx, SIZE_TYPE i) {
    SIZE_TYPE index;

    if (i >= ctx->begin + 1) {
        index = ctx->size + ctx->begin - i;
    } else {
        index = ctx->begin - i;
    }
    return index;
}

/* creating resource type on load */
static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info) {
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    circa_type = enif_open_resource_type(env, NULL, "circa",
                                         NULL, flags, NULL);
    if (circa_type == NULL) return 1;
    return 0;
}

static ERL_NIF_TERM
new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx* ctx;
    ERL_NIF_TERM ret;
    SIZE_TYPE size;

    if (argc != 1 || !ERL_GET_SIZE(env, argv[0], &size) ||
        size > MAX_SIZE || size == 0) {
        return enif_make_badarg(env);
    }

    ctx         = enif_alloc_resource(circa_type, sizeof(circactx));
    ctx->begin  = 0;
    ctx->circa  = enif_alloc(sizeof(ELEM_TYPE) * size);
    ctx->size   = size;
    ctx->filled = 0;

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
push(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    ELEM_TYPE val;

    if (argc != 2 || !ERL_GET_ELEM(env, argv[1], &val) ||
        !enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }

    if (++ctx->begin >= ctx->size + 1) {
        ctx->begin  = 1;
        ctx->filled = 1;
    }

    ctx->circa[ctx->begin - 1] = val;

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 enif_make_resource(env, ctx));
}

static ERL_NIF_TERM
get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    SIZE_TYPE i, idx;

    if (argc != 2 || !ERL_GET_SIZE(env, argv[1], &i) ||
        !enif_get_resource(env, argv[0], circa_type, (void**) &ctx) ||
        i > ctx->size || i == 0) {
        return enif_make_badarg(env);
    }
    if (!ctx->filled && i > ctx->begin) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                     enif_make_atom(env, "not_found"));
    }

    idx = getIndex(ctx, i);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 ERL_MAKE_ELEM(env, ctx->circa[idx]));
}

static ERL_NIF_TERM
set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    SIZE_TYPE i, idx;
    ELEM_TYPE val;

    if (argc != 3 || !ERL_GET_SIZE(env, argv[1], &i) ||
        !ERL_GET_ELEM(env, argv[2], &val) ||
        !enif_get_resource(env, argv[0], circa_type, (void**) &ctx) ||
        i > ctx->size || i == 0) {
        return enif_make_badarg(env);
    }
    if (!ctx->filled && i > ctx->begin) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                     enif_make_atom(env, "not_found"));
    }

    idx = getIndex(ctx, i);
    ctx->circa[idx] = val;

    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                 ERL_MAKE_ELEM(env, val));
}

static ERL_NIF_TERM
slice(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    SIZE_TYPE start, end, slicesize, idx, i, a;
    ERL_NIF_TERM * terms;

    if (argc != 3 || !ERL_GET_SIZE(env, argv[1], &start) ||
        !ERL_GET_SIZE(env, argv[2], &end) ||
        !enif_get_resource(env, argv[0], circa_type, (void**) &ctx) ||
        start > ctx->size || start == 0 ||
        end > ctx->size || end == 0 || start > end) {
        return enif_make_badarg(env);
    }

    slicesize = end - start + 1;

    if (slicesize > MAX_SLICE) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                enif_make_atom(env, "slice_too_big"));
    }
    if (!ctx->filled && (start > ctx->begin || end > ctx->begin)) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                     enif_make_atom(env, "not_found"));
    }

    /* create slice */
    terms = enif_alloc(sizeof(ERL_NIF_TERM) * slicesize);

    a = 0;
    for (i = start; i <= end; i++) {
        idx = getIndex(ctx, i);
        terms[a++] = ERL_MAKE_ELEM(env, ctx->circa[idx]);
    }

    return enif_make_list_from_array(env, terms, slicesize);
}

static ErlNifFunc functions[] =
{
    {"new", 1, new},
    {"push", 2, push},
    {"get", 2, get},
    {"set", 3, set},
    {"slice", 3, slice}
};

ERL_NIF_INIT(ecirca, functions, &load, NULL, NULL, NULL)
