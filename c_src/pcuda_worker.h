#ifndef PCUDA_WORKER
#define PCUDA_WORKER

#include <deque>
#include "erl_nif.h"
#include "pcuda_buffer.h"

enum PcudaCommandEnum {
    STOP_WORKER = 0,
    NEW_INT_BUFFER = 1,
    NEW_LONG_BUFFER = 2,
    NEW_FLOAT_BUFFER = 3,
    DESTROY_BUFFER = 4,
    GET_BUFFER_SIZE = 5,
    CLEAR_BUFFER = 6,
    APPEND_BUFFER = 7
};

class PcudaWorkerCommand {
public:
    enum PcudaCommandEnum cmd;
    bool done;
    void *result;

    PcudaWorkerCommand();
    virtual ~PcudaWorkerCommand();
};


class PcudaWorker {

private:
    ErlNifTid tid;
    ErlNifMutex *guard;
    ErlNifCond *flag;
    ErlNifCond *resultFlag;
    ErlNifEnv *env;
    bool threadRunning;
    std::deque<PcudaWorkerCommand *> queue;
    PcudaIntBuffer *buffer;

    static void *thunk(void *args);

    void *loop();
    const bool handleCommand(PcudaWorkerCommand *cmd);
    const bool setupEnvironment();
    void teardownEnvironment();

    const bool createIntBuffer(PcudaWorkerCommand *cmd);
    const bool destroyBuffer(PcudaWorkerCommand *cmd);
    const bool handleShutdown(PcudaWorkerCommand *cmd);

public:
    PcudaWorker();
    virtual ~PcudaWorker();
    const bool startThread();
    void stopThread();
    void enqueueCommand(PcudaWorkerCommand *cmd);
};

typedef struct _pcuda_worker_handle {
    PcudaWorker *ref;
} pcuda_worker_handle;

#endif
