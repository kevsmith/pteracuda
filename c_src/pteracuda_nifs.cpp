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

    ERL_NIF_TERM pteracuda_nifs_new_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_destroy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

    ERL_NIF_TERM pteracuda_nifs_write_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_read_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_sort_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

    static ErlNifFunc pteracuda_nif_funcs[] = {
        {"new_buffer", 0, pteracuda_nifs_new_buffer},
        {"destroy_buffer", 1, pteracuda_nifs_destroy_buffer},
        {"buffer_size", 1, pteracuda_nifs_buffer_size},
        {"write_buffer", 2, pteracuda_nifs_write_buffer},
        {"read_buffer", 1, pteracuda_nifs_read_buffer},
        {"sort_buffer", 1, pteracuda_nifs_sort_buffer}
    };
}

static ErlNifResourceType *pteracuda_buffer_resource;

struct PCudaBufferRef {
    PCudaBuffer *buffer;
    CUcontext ctx;
};

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_WRONG_TYPE;
static ERL_NIF_TERM OOM_ERROR;

ERL_NIF_INIT(pteracuda_nifs, pteracuda_nif_funcs, &pteracuda_on_load, NULL, NULL, NULL);

static int pteracuda_on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    if (cuInit(0) == CUDA_SUCCESS) {
        ATOM_OK = enif_make_atom(env, "ok");
        ATOM_ERROR = enif_make_atom(env, "error");
        ATOM_WRONG_TYPE = enif_make_atom(env, "wrong_type");
        pteracuda_buffer_resource = enif_open_resource_type(env, NULL, "pteracuda_buffer_resource",
                                                            NULL, ERL_NIF_RT_CREATE, 0);
        /* Pre-alloate OOM error in case we run out of memory later */
        OOM_ERROR = enif_make_tuple2(env, ATOM_ERROR, enif_make_atom(env, "out_of_memory"));
        return 0;
    }
    else {
        return -1;
    }
}

ERL_NIF_TERM pteracuda_nifs_new_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    CUdevice device;
    PCudaBufferRef *ref = (PCudaBufferRef *) enif_alloc_resource(pteracuda_buffer_resource, sizeof(PCudaBufferRef));
    if (!ref) {
        return OOM_ERROR;
    }
    cuDeviceGet(&device, 0);
    cuCtxCreate(&(ref->ctx), CU_CTX_SCHED_AUTO, device);
    ref->buffer = new PCudaIntBuffer();
    ERL_NIF_TERM res = enif_make_resource(env, ref);
    enif_release_resource(ref);
    return enif_make_tuple2(env, ATOM_OK, res);
}

ERL_NIF_TERM pteracuda_nifs_destroy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    delete ref->buffer;
    cuCtxDestroy(ref->ctx);
    //cudaThreadExit();
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

ERL_NIF_TERM pteracuda_nifs_buffer_size(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    return enif_make_tuple2(env, ATOM_OK, enif_make_long(env, ref->buffer->size()));
}

ERL_NIF_TERM pteracuda_nifs_sort_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    PCudaBufferRef *ref;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_buffer_resource, (void **) &ref)) {
        return enif_make_badarg(env);
    }
    cuCtxSetCurrent(ref->ctx);
    if (ref->buffer->type() != BUF_TYPE_INTEGER) {
        return enif_make_tuple2(env, ATOM_ERROR, ATOM_WRONG_TYPE);
    }
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
