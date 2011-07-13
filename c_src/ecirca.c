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

//data structure
typedef struct {
    SIZE_TYPE           begin;
	ELEM_TYPE           *circa;
    SIZE_TYPE           size;
    unsigned short int  filled;
} circactx;

ErlNifResourceType* circa_type;

//creating resource on load
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
    ERL_NIF_TERM ret;
    SIZE_TYPE size;
    
    if (argc != 1 || !ERL_GET_SIZE(env, argv[0], &size)) {
        return enif_make_badarg(env);
    }
    if (size > MAX_SIZE || size == 0) {
        return enif_make_badarg(env);
    }

    circactx* ctx = enif_alloc_resource(circa_type, sizeof(circactx));
    ctx->begin  = 0;
    ctx->circa  = enif_alloc(sizeof(ELEM_TYPE) * size);
    ctx->size   = size;
    ctx->filled = 0;
    
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
   
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), 
                                 ret);
}


static ERL_NIF_TERM
push(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    ELEM_TYPE val;
    
    if (argc != 2 || !ERL_GET_ELEM(env, argv[1], &val)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }
    
    if (++ctx->begin >= ctx->size + 1) {
        ctx->begin  = 1;
        ctx->filled = 1;
    }
    
    ctx->circa[ctx->begin - 1] = val;
    
    return enif_make_atom(env, "ok");
}


static ERL_NIF_TERM
get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    SIZE_TYPE i;
    SIZE_TYPE idx;
    
    if (argc != 2 || !ERL_GET_SIZE(env, argv[1], &i)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }
    if (i > ctx->size || i == 0) {
        return enif_make_badarg(env);
    }
    if (!ctx->filled && i > ctx->begin) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                     enif_make_atom(env, "not_found"));
    }
    
    if (i >= ctx->begin + 1) {
        idx = ctx->size + ctx->begin - i;
    } else {
        idx = ctx->begin - i;
    }
    
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), 
                                 ERL_MAKE_ELEM(env, ctx->circa[idx]));
}


static ERL_NIF_TERM
set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    circactx * ctx;
    SIZE_TYPE i, idx;
    ELEM_TYPE val;
    
    if (argc != 3 || !ERL_GET_SIZE(env, argv[1], &i) || 
        !ERL_GET_ELEM(env, argv[2], &val)) {
        return enif_make_badarg(env);
    }
    if (!enif_get_resource(env, argv[0], circa_type, (void**) &ctx)) {
        return enif_make_badarg(env);
    }
    if (i > ctx->size || i == 0) {
        return enif_make_badarg(env);
    }
    if (!ctx->filled && i > ctx->begin) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), 
                                     enif_make_atom(env, "not_found"));
    }
    
    if (i >= ctx->begin + 1) {
        idx = ctx->size + ctx->begin - i;
    } else {
        idx = ctx->begin - i;
    }
    
    ctx->circa[idx] = val;
    
    return enif_make_atom(env, "ok");
}


//functions
static ErlNifFunc functions[] =
{
    {"new", 1, new},
    {"push", 2, push},
    {"get", 2, get},
    {"set", 3, set}
};

ERL_NIF_INIT(ecirca, functions, &load, NULL, NULL, NULL);
