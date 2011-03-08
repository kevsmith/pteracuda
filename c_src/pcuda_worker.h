#ifndef PCUDA_WORKER
#define PCUDA_WORKER

#include <vector>
#include <deque>
#include "erl_nif.h"
#include "pcuda_buffer.h"

enum PcudaCommandEnum {
    STOP_WORKER = 0,
    NEW_INT_BUFFER = 1,
    DESTROY_BUFFER = 2,
    GET_BUFFER_SIZE = 3,
    CLEAR_BUFFER = 4,
    APPEND_BUFFER = 5,
    READ_BUFFER = 6
};

class PcudaWorkerCommand {
public:
    enum PcudaCommandEnum cmd;
    bool done;
    void *arg;
    void *result;
    bool freeResult;

    PcudaWorkerCommand();
    PcudaWorkerCommand(PcudaCommandEnum cmd);
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
    PcudaLongBuffer *buffer;

    static void *thunk(void *args);

    void *loop();
    const bool handleCommand(PcudaWorkerCommand *cmd);
    const bool setupEnvironment();
    void teardownEnvironment();

    const bool createIntBuffer(PcudaWorkerCommand *cmd);
    const bool destroyBuffer(PcudaWorkerCommand *cmd);
    const bool handleShutdown(PcudaWorkerCommand *cmd);
    const bool appendBuffer(PcudaWorkerCommand *cmd);
    const bool readBuffer(PcudaWorkerCommand *cmd);
    const bool getBufferSize(PcudaWorkerCommand *cmd);

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
