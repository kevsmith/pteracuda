// -------------------------------------------------------------------
//
// pteracuda: An Erlang framework for performing CUDA-enabled operations
//
// Copyright (c) 2011 Hypothetical Labs, Inc. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#include <stdio.h>

#include "cuda.h"
#include "cuda_runtime_api.h"
#include "erl_nif.h"

#include "pcuda_buffer.h"

extern "C" {
    static int pteracuda_on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);

    ERL_NIF_TERM pteracuda_nifs_new_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_destroy_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM pteracuda_nifs_new_int_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_new_string_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_new_float_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM pteracuda_nifs_destroy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM pteracuda_nifs_write_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_read_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_delete(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_insert(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_sort_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_clear_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_contains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_copy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_intersection(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_minmax(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

    static ErlNifFunc pteracuda_nif_funcs[] = {
        {"new_context", 0, pteracuda_nifs_new_context},
        {"new_context", 1, pteracuda_nifs_new_context},
        {"destroy_context", 1, pteracuda_nifs_destroy_context},
        {"new_int_buffer", 0, pteracuda_nifs_new_int_buffer},
        {"new_string_buffer", 0, pteracuda_nifs_new_string_buffer},
        {"new_float_buffer", 0, pteracuda_nifs_new_float_buffer},
        {"destroy_buffer", 1, pteracuda_nifs_destroy_buffer},
        {"buffer_size", 1, pteracuda_nifs_buffer_size},
        {"write_buffer", 2, pteracuda_nifs_write_buffer},
        {"buffer_delete", 2, pteracuda_nifs_buffer_delete},
        {"buffer_insert", 3, pteracuda_nifs_buffer_insert},
        {"read_buffer", 1, pteracuda_nifs_read_buffer},
        {"sort_buffer", 2, pteracuda_nifs_sort_buffer},
        {"clear_buffer", 1, pteracuda_nifs_clear_buffer},
        {"buffer_contains", 3, pteracuda_nifs_buffer_contains},
        {"copy_buffer", 2, pteracuda_nifs_copy_buffer},
        {"buffer_intersection", 3, pteracuda_nifs_buffer_intersection},
        {"buffer_minmax", 2, pteracuda_nifs_buffer_minmax}
    };
}

static ErlNifResourceType *pteracuda_buffer_resource;
static ErlNifResourceType *pteracuda_context_resource;

struct PCudaBufferRef {
    PCudaBuffer *buffer;
    bool destroyed;
};

struct PCudaContextRef {
    CUcontext ctx;
    bool destroyed;
};

static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_WRONG_TYPE;
static ERL_NIF_TERM OOM_ERROR;

ERL_NIF_INIT(pteracuda_nifs, pteracuda_nif_funcs, &pteracuda_on_load, NULL, NULL, NULL);

static int pteracuda_on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    if (cuInit(0) == CUDA_SUCCESS) {
        ATOM_TRUE = enif_make_atom(env, "true");
        ATOM_FALSE = enif_make_atom(env, "false");
        ATOM_OK = enif_make_atom(env, "ok");
        ATOM_ERROR = enif_make_atom(env, "error");
        ATOM_WRONG_TYPE = enif_make_atom(env, "wrong_type");
        pteracuda_buffer_resource = enif_open_resource_type(env, NULL, "pteracuda_buffer_resource",
                                                            NULL, ERL_NIF_RT_CREATE, 0);
        pteracuda_context_resource = enif_open_resource_type(env, NULL, "pteracuda_context_resource",
                                                             NULL, ERL_NIF_RT_CREATE, 0);
        /* Pre-alloate OOM error in case we run out of memory later */
        OOM_ERROR = enif_make_tuple2(env, ATOM_ERROR, enif_make_atom(env, "out_of_memory"));
        return 0;
    }
    else {
        return -1;
    }
}

ERL_NIF_TERM pteracuda_nifs_new_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    CUdevice device;
    int deviceNum = 0;
    PCudaContextRef *ref = (PCudaContextRef *) enif_alloc_resource(pteracuda_context_resource, sizeof(PCudaContextRef));
    if (!ref) {
        return OOM_ERROR;
    }
    if (argc == 1 && !enif_get_int(env, argv[0], &deviceNum)) {
        return enif_make_badarg(env);
    }
    if (cuDeviceGet(&device, deviceNum) == CUDA_SUCCESS &&
        cuCtxCreate(&(ref->ctx), CU_CTX_SCHED_AUTO, device) == CUDA_SUCCESS) {
        ref->destroyed = false;
        ERL_NIF_TERM result = enif_make_resource(env, ref);
        enif_release_resource(ref);
        return enif_make_tuple2(env, ATOM_OK, result);
    }
    else {
        return ATOM_ERROR;
    }
}

ERL_NIF_TERM pteracuda_nifs_destroy_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaContextRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_context_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    if (!ref->destroyed) {
        cuCtxDestroy(ref->ctx);
        ref->destroyed = true;
    }
    return ATOM_OK;
}

ERL_NIF_TERM pteracuda_nifs_new_int_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref = (PCudaBufferRef *) enif_alloc_resource(pteracuda_buffer_resource, sizeof(PCudaBufferRef));
    if (!ref) {
        return OOM_ERROR;
    }
    ref->buffer = new PCudaIntBuffer();
    ref->destroyed = false;
    ERL_NIF_TERM res = enif_make_resource(env, ref);
    enif_release_resource(ref);
    return enif_make_tuple2(env, ATOM_OK, res);
}

ERL_NIF_TERM pteracuda_nifs_new_string_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref = (PCudaBufferRef *) enif_alloc_resource(pteracuda_buffer_resource, sizeof(PCudaBufferRef));
    if (!ref) {
        return OOM_ERROR;
    }
    ref->buffer = new PCudaStringBuffer();
    ref->destroyed = false;
    ERL_NIF_TERM res = enif_make_resource(env, ref);
    enif_release_resource(ref);
    return enif_make_tuple2(env, ATOM_OK, res);
}

ERL_NIF_TERM pteracuda_nifs_new_float_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref = (PCudaBufferRef *) enif_alloc_resource(pteracuda_buffer_resource, sizeof(PCudaBufferRef));
    if (!ref) {
        return OOM_ERROR;
    }
    ref->buffer = new PCudaFloatBuffer();
    ref->destroyed = false;
    ERL_NIF_TERM res = enif_make_resource(env, ref);
    enif_release_resource(ref);
    return enif_make_tuple2(env, ATOM_OK, res);
}

ERL_NIF_TERM pteracuda_nifs_destroy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    if (!ref->destroyed) {
        delete ref->buffer;
        ref->destroyed = true;
    }
    return ATOM_OK;
}

ERL_NIF_TERM pteracuda_nifs_write_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 2 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    ref->buffer->write(env, argv[1]);
    return ATOM_OK;
}

ERL_NIF_TERM pteracuda_nifs_buffer_delete(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    unsigned long position;
    if (argc != 2 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref) ||
        !enif_get_ulong(env, argv[1], &position)) {
        return enif_make_badarg(env);
    }
    if (position > ref->buffer->size()) {
        return ATOM_ERROR;
    }
    ref->buffer->delete_at(position);
    return ATOM_OK;
}

ERL_NIF_TERM pteracuda_nifs_buffer_insert(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    unsigned long position;
    if (argc != 3 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref) ||
        !enif_get_ulong(env, argv[1], &position)) {
        return enif_make_badarg(env);
    }
    if (position > ref->buffer->size()) {
        return ATOM_ERROR;
    }
    if (ref->buffer->insert_at(position, env, argv[2])) {
        return ATOM_OK;
    }
    else {
        return ATOM_ERROR;
    }
}

ERL_NIF_TERM pteracuda_nifs_buffer_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, ref->buffer->size()));
}

ERL_NIF_TERM pteracuda_nifs_sort_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaContextRef *ctxRef;
    PCudaBufferRef *ref;
    if (argc != 2 || !enif_get_resource(env, argv[0], pteracuda_context_resource, (void **) &ctxRef) ||
        !enif_get_resource(env, argv[1], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    cuCtxSetCurrent(ctxRef->ctx);
    if (ref->buffer->sort()) {
        return ATOM_OK;
    }
    else {
        return ATOM_ERROR;
    }
}

ERL_NIF_TERM pteracuda_nifs_read_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    ERL_NIF_TERM data = ref->buffer->toErlTerms(env);
    return enif_make_tuple2(env, ATOM_OK, data);
}

ERL_NIF_TERM pteracuda_nifs_clear_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    ref->buffer->clear();
    return ATOM_OK;
}

ERL_NIF_TERM pteracuda_nifs_buffer_contains(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaContextRef *ctxRef;
    PCudaBufferRef *ref;
    if (argc !=3 || !enif_get_resource(env, argv[0], pteracuda_context_resource, (void **) &ctxRef) ||
        !enif_get_resource(env, argv[1], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    if (ref->buffer->size() > 0) {
        cuCtxSetCurrent(ctxRef->ctx);
        if (ref->buffer->contains(env, argv[2])) {
            return ATOM_TRUE;
        }
        else {
            return ATOM_FALSE;
        }
    }
    else {
        return ATOM_FALSE;
    }
}

ERL_NIF_TERM pteracuda_nifs_copy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *src, *dest;
    if (argc !=2 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &src) ||
        !enif_get_resource(env, argv[1], pteracuda_buffer_resource, (void **) &dest)) {
        return enif_make_badarg(env);
    }

    if (dest->buffer->copy(src->buffer)) {
        return ATOM_OK;
    }
    else {
        return ATOM_ERROR;
    }
}

ERL_NIF_TERM pteracuda_nifs_buffer_intersection(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaContextRef *ctxRef;
    PCudaBufferRef *first, *second;
    if (argc !=3 || !enif_get_resource(env, argv[0], pteracuda_context_resource, (void **) &ctxRef) ||
        !enif_get_resource(env, argv[1], pteracuda_buffer_resource, (void **) &first) ||
        !enif_get_resource(env, argv[2], pteracuda_buffer_resource, (void **) &second)) {
        return enif_make_badarg(env);
    }
    cuCtxSetCurrent(ctxRef->ctx);
    return enif_make_tuple2(env, ATOM_OK, first->buffer->intersect(env, second->buffer));
}

ERL_NIF_TERM pteracuda_nifs_buffer_minmax(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaContextRef *ctxRef;
    PCudaBufferRef *bufRef;
    if (argc !=2 || !enif_get_resource(env, argv[0], pteracuda_context_resource, (void **) &ctxRef) ||
        !enif_get_resource(env, argv[1], pteracuda_buffer_resource, (void **) &bufRef)) {
        return enif_make_badarg(env);
    }
    if (bufRef->buffer->size() == 0) {
        return enif_make_tuple2(env, ATOM_OK, enif_make_tuple2(env, enif_make_int(env, 0),
                                                               enif_make_int(env, 0)));
    }
    cuCtxSetCurrent(ctxRef->ctx);
    return enif_make_tuple2(env, ATOM_OK, bufRef->buffer->minmax(env));
}
