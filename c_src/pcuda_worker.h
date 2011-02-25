#ifndef PCUDA_WORKER
#define PCUDA_WORKER

#include <deque>
#include "erl_nif.h"

enum PcudaCommandEnum {
    STOP_WORKER = 0,
    NEW_BUFFER = 1,
    DESTROY_BUFFER = 2,
    GET_BUFFER_SIZE = 3,
    CLEAR_BUFFER = 4,
    APPEND_BUFFER = 5
};

class PcudaWorkerCommand {
public:
    ErlNifPid *caller;
    enum PcudaCommandEnum cmd;
};

class PcudaWorker {

private:
    ErlNifTid tid;
    ErlNifMutex *guard;
    ErlNifCond *flag;
    ErlNifEnv *env;
    bool threadRunning;
    std::deque<PcudaWorkerCommand *> queue;

    static void *thunk(void *args);

    void *loop();
    const bool setupEnvironment();
    void teardownEnvironment();

public:
    PcudaWorker();
    virtual ~PcudaWorker();
    const bool startThread();
    void stopThread();
    const bool enqueueCommand(PcudaWorkerCommand *cmd);
};

typedef struct _pcuda_worker_handle {
    PcudaWorker *ref;
} pcuda_worker_handle;

#endif