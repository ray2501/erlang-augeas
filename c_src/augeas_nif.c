#include "erl_nif.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <augeas.h>
    
ERL_NIF_TERM mk_atom(ErlNifEnv* env, const char* atom);
ERL_NIF_TERM mk_error(ErlNifEnv* env, const char* mesg);

static ErlNifResourceType* RES_TYPE;

#define MAXBUFLEN 1024

typedef struct {
    augeas *aug;
} AUGEAS;

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
    ERL_NIF_TERM ret;

    if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
        return enif_make_atom(env, atom);
    }

    return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
    return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

static void
free_res(ErlNifEnv* env, void* obj)
{
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    ErlNifResourceType* rt = enif_open_resource_type(env, "augeas", "augeas_res", 
                                                     free_res,
                                                     ERL_NIF_RT_CREATE, NULL);    

    if(rt == NULL) return -1;
    assert(rt != NULL);
    RES_TYPE = rt;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static ERL_NIF_TERM
newaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    ERL_NIF_TERM ret;
    char root[MAXBUFLEN];
    char loadpath[MAXBUFLEN];
    unsigned int flags = 0; 
    augeas *aug = NULL;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&root, '\0', sizeof(root));
    (void)memset(&loadpath, '\0', sizeof(loadpath));

    if (enif_get_string(env, argv[0], root, sizeof(root), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], loadpath, sizeof(loadpath), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[2], &flags))
    {
        return enif_make_badarg(env);
    }

    aug = aug_init(root, loadpath, flags);
    if(!aug) return enif_make_badarg(env);

    res = enif_alloc_resource(RES_TYPE, sizeof(AUGEAS));
    if(res == NULL) return mk_error(env, "alloc_error");
    res->aug = NULL;

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->aug = aug;

    return enif_make_tuple2(env, mk_atom(env, "ok"), ret); 
}

static ERL_NIF_TERM
getaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char path[MAXBUFLEN];
    const char* value;
    int aug_result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&path, '\0', sizeof(path));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_get(res->aug, path, &value);
    if (aug_result == 1) {
        if(!value) {
            return enif_make_string(env, "", ERL_NIF_LATIN1);
        }

        return enif_make_string(env, value, ERL_NIF_LATIN1);
    }

    return mk_error(env, "get_error");
}

static ERL_NIF_TERM
setaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char path[MAXBUFLEN];
    char value[MAXBUFLEN];
    int aug_result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&path, '\0', sizeof(path));
    (void)memset(&value, '\0', sizeof(value));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[2], value, sizeof(value), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }    
    
    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_set(res->aug, path, value);
    if (aug_result == 0) {
        return mk_atom(env, "ok");
    }

    return mk_error(env, "set_error");
}

static ERL_NIF_TERM
setmaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char base[MAXBUFLEN];
    char sub[MAXBUFLEN];
    char value[MAXBUFLEN];
    int aug_result = 0;

    if(argc != 4)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&base, '\0', sizeof(base));
    (void)memset(&sub, '\0', sizeof(sub));
    (void)memset(&value, '\0', sizeof(value));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], base, sizeof(base), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[2], sub, sizeof(sub), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[3], value, sizeof(value), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_setm(res->aug, base, sub, value);
    if (aug_result >= 0) {
        return enif_make_int(env, aug_result);
    }

    return mk_error(env, "setm_error");
}

static ERL_NIF_TERM
insertaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char path[MAXBUFLEN];
    char label[MAXBUFLEN];
    int before = 0;
    int aug_result = 0;

    if(argc != 4)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&path, '\0', sizeof(path));
    (void)memset(&label, '\0', sizeof(label));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[2], label, sizeof(label), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (!enif_get_int(env, argv[3], &before))
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_insert(res->aug, path, label, before);
    if (aug_result == 0) {
        return mk_atom(env, "ok");
    }

    return mk_error(env, "insert_error");
}

static ERL_NIF_TERM
rmaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char path[MAXBUFLEN];
    int aug_result = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&path, '\0', sizeof(path));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_rm(res->aug, path);
    if (aug_result >= 0) {
        return enif_make_int(env, aug_result);
    }

    return mk_error(env, "rm_error");
}

static ERL_NIF_TERM
mvaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char src[MAXBUFLEN];
    char dst[MAXBUFLEN];
    int aug_result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&src, '\0', sizeof(src));
    (void)memset(&dst, '\0', sizeof(dst));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], src, sizeof(src), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[2], dst, sizeof(dst), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_mv(res->aug, src, dst);
    if (aug_result == 0) {
        return mk_atom(env, "ok");
    }

    return mk_error(env, "mv_error");
}

static ERL_NIF_TERM
cpaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char src[MAXBUFLEN];
    char dst[MAXBUFLEN];
    int aug_result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&src, '\0', sizeof(src));
    (void)memset(&dst, '\0', sizeof(dst));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], src, sizeof(src), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[2], dst, sizeof(dst), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_cp(res->aug, src, dst);
    if (aug_result == 0) {
        return mk_atom(env, "ok");
    }

    return mk_error(env, "cp_error");
}

static ERL_NIF_TERM
renameaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char src[MAXBUFLEN];
    char lbl[MAXBUFLEN];
    int aug_result = 0;

    if(argc != 3)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&src, '\0', sizeof(src));
    (void)memset(&lbl, '\0', sizeof(lbl));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], src, sizeof(src), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[2], lbl, sizeof(lbl), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_rename(res->aug, src, lbl);
    if (aug_result >= 0) {
        return enif_make_int(env, aug_result);
    }

    return mk_error(env, "rename_error");
}

static ERL_NIF_TERM
matchaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS* res;
    char path[MAXBUFLEN];
    char** matches = NULL;
    int aug_result = 0;
    ERL_NIF_TERM erl_list;
    int i = 0;

    if(argc != 2)
    {
        return enif_make_badarg(env);
    }

    (void)memset(&path, '\0', sizeof(path));

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if (enif_get_string(env, argv[1], path, sizeof(path), ERL_NIF_LATIN1) < 1)
    {
        return enif_make_badarg(env);
    }

    if(res->aug == NULL) {
        return mk_error(env, "resource_error");
    }

    aug_result = aug_match(res->aug, path, &matches);
    if (aug_result >= 0) {
        erl_list = enif_make_list(env, 0);

        if (aug_result > 0) {
            for (i = 0; i < aug_result; i++)
            {
                if(matches[i]) {
                    erl_list = enif_make_list_cell(env,
                                                   enif_make_string(env, matches[i], ERL_NIF_LATIN1),
                                                   erl_list);
                    free(matches[i]);
                } else {
                    erl_list = enif_make_list_cell(env,
                                                   enif_make_string(env, "", ERL_NIF_LATIN1),
                                                   erl_list);
                }
            }
        }
        free(matches);

        return erl_list;
    }

    return mk_error(env, "match_error");
}

static ERL_NIF_TERM
saveaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS *res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->aug) {
        aug_save(res->aug);
    } else {
        return mk_error(env, "resource_error");
    }

    return mk_atom(env, "ok");
}

static ERL_NIF_TERM
closeaugeas(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    AUGEAS *res;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_resource(env, argv[0], RES_TYPE, (void **) &res))
    {
        return enif_make_badarg(env);
    }

    if(res->aug) {
        aug_close(res->aug);
        res->aug = NULL;
        assert(res->aug == NULL);
    }

    return mk_atom(env, "ok");
}


static ErlNifFunc nif_funcs[] = {
    {"new", 3, newaugeas, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"get", 2, getaugeas},
    {"set", 3, setaugeas},
    {"setm", 4, setmaugeas},
    {"insert", 4, insertaugeas},
    {"rm", 2, rmaugeas},
    {"mv", 3, mvaugeas},
    {"cp", 3, cpaugeas},
    {"rename", 3, renameaugeas},
    {"match", 2, matchaugeas},
    {"save", 1, saveaugeas, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"close", 1, closeaugeas}
};

ERL_NIF_INIT(augeas, nif_funcs, &load, &reload, NULL, NULL)

