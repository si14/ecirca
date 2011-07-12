#include "erl_nif.h"
#include <inttypes.h>

#define MAXSLICE    10
#define EMPTYVAL    -1
#define MAXSIZE     100

//data structure
typedef struct {
    uint32_t    begin;
	uint64_t    *circa;
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

//create new 
static ERL_NIF_TERM
new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM ret;
    int size;
    
    if (argc != 1 || !enif_get_int(env, argv[0], &size) {
        return enif_make_badarg(env);
    }
    if (size > MAXSIZE || size <= 0) {
        return enif_make_badarg(env);
    }

    circactx* ctx = enif_alloc_resource(circa_type, sizeof(circactx));
    ctx->circa = enif_alloc(sizeof(uint64_t) * size);
    
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
   
    return ret;
}


//functions
static ErlNifFunc functions[] =
{
    {"new", 1, new}
};

ERL_NIF_INIT(ecirca, functions, &load, NULL, NULL, NULL);
