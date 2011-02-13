#ifndef PCDUA_WORKER
#define PCUDA_WORKER

#include "erl_nif.h"
#include <deque>
#include <string>

typedef struct _pcuda_command {
    int cmd;
    void *args;
    ErlNifPid caller;
} pcuda_command;

typedef struct _pcuda_worker {
    int running;
    ErlNifTid tid;
    ErlNifMutex *command_guard;
    ErlNifCond *command_flag;
    ErlNifEnv *env;
    std::deque<pcuda_command *> *queue;
} pcuda_worker;

int pcuda_create_worker(pcuda_worker *worker);
void pcuda_destroy_worker(pcuda_worker *worker);
void pcuda_enqueue_command(pcuda_worker *worker, pcuda_command *command);

#endif
