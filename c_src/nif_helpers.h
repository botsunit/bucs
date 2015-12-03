#define NIF(name) static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

#define VALIDATE_ARITY(n) if(argc < n) { return enif_make_badarg(env); }

#define GET_ARG_STRING(__pos, __string, __size, __error, __result, __goto) \
  if(!enif_get_list_length(env, argv[__pos], &__size)) {\
    __result = enif_make_tuple2(env, enif_make_atom(env, "error"), __error);\
    goto __goto;\
  }\
  __string = malloc(__size + 1);\
  if(__string == NULL) {\
    __result = enif_make_tuple2(env, enif_make_atom(env, "error"), __error);\
    goto __goto;\
  }\
  if(enif_get_string(env, argv[__pos], __string, __size+1, ERL_NIF_LATIN1) < 1) {\
    __result = enif_make_tuple2(env, enif_make_atom(env, "error"), __error);\
    goto __goto;\
  }

#define GET_ARG_UINT(__pos, __int, __error, __result, __goto) \
  if(!enif_get_uint(env, argv[__pos], &__int)) {\
    __result = enif_make_tuple2(env, enif_make_atom(env, "error"), __error);\
    goto __goto;\
  }

#define FREE(__x) if(__x) {free(__x);}
