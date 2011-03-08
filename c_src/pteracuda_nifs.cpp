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
#include <string.h>
#include <vector>

#include "erl_nif.h"
#include "pteracuda_common.h"
#include "pcuda_worker.h"
#include "pcuda_buffer.h"

extern "C" {
    static int pteracuda_on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info);

    ERL_NIF_TERM pteracuda_nifs_new_worker(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_destroy_worker(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_new_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_destroy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_buffer_length(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_write_integers(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM pteracuda_nifs_read_integers(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

    static ErlNifFunc pteracuda_nif_funcs[] = {
        {"new_worker", 0, pteracuda_nifs_new_worker},
        {"destroy_worker", 1, pteracuda_nifs_destroy_worker},
        {"new_buffer", 1, pteracuda_nifs_new_buffer},
        {"destroy_buffer", 1, pteracuda_nifs_destroy_buffer},
        {"write_integers", 2, pteracuda_nifs_write_integers},
        {"read_integers", 1, pteracuda_nifs_read_integers},
        {"buffer_length", 1, pteracuda_nifs_buffer_length}
    };
}

ERL_NIF_INIT(pteracuda_nifs, pteracuda_nif_funcs, &pteracuda_on_load, NULL, NULL, NULL);

static int pteracuda_on_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");
    pteracuda_worker_resource = enif_open_resource_type(env, NULL, "pteracuda_worker_resource",
                                                        NULL, ERL_NIF_RT_CREATE, 0);
    /* Pre-alloate OOM error in case we run out of memory later */
    OOM_ERROR = enif_make_tuple2(env, ATOM_ERROR, enif_make_atom(env, "out_of_memory"));
    return 0;
}

ERL_NIF_TERM pteracuda_nifs_new_worker(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval = ATOM_ERROR;
    PcudaWorker *worker = new PcudaWorker();
    pcuda_worker_handle *handle = (pcuda_worker_handle *) enif_alloc_resource(pteracuda_worker_resource,
                                                                              sizeof(pcuda_worker_handle));
    if (handle == NULL) {
        delete worker;
        return OOM_ERROR;
    }
    if (worker->startThread()) {
        handle->ref = worker;
        ERL_NIF_TERM result = enif_make_resource(env, handle);
        enif_release_resource(handle);
        retval = enif_make_tuple2(env, ATOM_OK, result);
    }
    else {
        delete worker;
        enif_release_resource(handle);
        retval = ATOM_ERROR;
    }
    return retval;
}

ERL_NIF_TERM pteracuda_nifs_destroy_worker(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    pcuda_worker_handle *handle;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_worker_resource, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    if (handle->ref == NULL) {
        return ATOM_ERROR;
    }
    PcudaWorker *worker = handle->ref;
    worker->stopThread();
    delete worker;
    handle->ref = NULL;
    return ATOM_OK;
}

ERL_NIF_TERM pteracuda_nifs_new_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval;
    pcuda_worker_handle *handle;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_worker_resource, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    PcudaWorker *worker = handle->ref;
    PcudaWorkerCommand *cmd = new PcudaWorkerCommand();
    cmd->cmd = NEW_INT_BUFFER;
    worker->enqueueCommand(cmd);
    bool result = (bool) cmd->result;
    if (result == true) {
        retval = ATOM_OK;
    }
    else {
        retval = ATOM_ERROR;
    }
    delete cmd;
    return retval;
}

ERL_NIF_TERM pteracuda_nifs_destroy_buffer(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval;
    pcuda_worker_handle *handle;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_worker_resource, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    PcudaWorker *worker = handle->ref;
    PcudaWorkerCommand *cmd = new PcudaWorkerCommand();
    cmd->cmd = DESTROY_BUFFER;
    worker->enqueueCommand(cmd);
    bool result = (bool) cmd->result;
    if (result == true) {
        retval = ATOM_OK;
    }
    else {
        retval = ATOM_ERROR;
    }
    delete cmd;
    return retval;
}

ERL_NIF_TERM pteracuda_nifs_buffer_length(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval;
    pcuda_worker_handle *handle;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_worker_resource, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    PcudaWorker *worker = handle->ref;
    PcudaWorkerCommand *cmd = new PcudaWorkerCommand();
    cmd->cmd = GET_BUFFER_SIZE;
    worker->enqueueCommand(cmd);
    long result = (long) cmd->result;
    retval = enif_make_tuple2(env, ATOM_OK, enif_make_long(env, result));
    delete cmd;
    return retval;
}

ERL_NIF_TERM pteracuda_nifs_write_integers(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval;
    long currentValue;
    pcuda_worker_handle *handle;
    if (argc != 2 || !enif_get_resource(env, argv[0], pteracuda_worker_resource, (void **) &handle) ||
        !enif_is_list(env, argv[1])) {
        return enif_make_badarg(env);
    }
    PcudaWorker *worker = handle->ref;
    std::vector<long> *values = new std::vector<long>();
    ERL_NIF_TERM head, tail;
    enif_get_list_cell(env, argv[1], &head, &tail);
    do {
        enif_get_long(env, head, &currentValue);
        values->push_back(currentValue);
    } while (enif_get_list_cell(env, tail, &head, &tail));
    PcudaWorkerCommand *cmd = new PcudaWorkerCommand();
    cmd->cmd = APPEND_BUFFER;
    cmd->arg = values;
    worker->enqueueCommand(cmd);
    bool result = (bool) cmd->result;
    if (result == true) {
        retval = ATOM_OK;
    }
    else {
        retval = ATOM_ERROR;
    }
    delete values;
    delete cmd;
    return retval;
}

ERL_NIF_TERM pteracuda_nifs_read_integers(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM retval;
    pcuda_worker_handle *handle;
    if (argc != 1 || !enif_get_resource(env, argv[0], pteracuda_worker_resource, (void **) &handle)) {
        return enif_make_badarg(env);
    }
    PcudaWorker *worker = handle->ref;
    PcudaWorkerCommand *cmd = new PcudaWorkerCommand(READ_BUFFER);
    std::vector<long> *values = new std::vector<long>();
    cmd->arg = (void *) values;
    worker->enqueueCommand(cmd);
    retval = enif_make_list(env, 0);
    delete values;
    delete cmd;
    return enif_make_tuple2(env, ATOM_OK, retval);
}
