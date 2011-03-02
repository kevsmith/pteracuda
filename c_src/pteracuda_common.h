#ifndef PCUDA_COMMON
#define PCUDA_COMMON

#include "erl_nif.h"
#include "pteracuda_util.h"

static ErlNifResourceType *pteracuda_worker_resource;

static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM OOM_ERROR;

#endif
