#include <unistd.h>
#include <stdio.h>
#include <vector>
#include "cuda_runtime_api.h"
#include "erl_nif.h"

#include "pteracuda_common.h"
#include "pcuda_worker.h"

PcudaWorkerCommand::PcudaWorkerCommand() {
    this->result = NULL;
    this->arg = NULL;
    this->done = false;
    this->freeResult = false;
}

PcudaWorkerCommand::~PcudaWorkerCommand() {
    if (this->result != NULL && this->freeResult) {
        free(this->result);
    }
}

PcudaWorker::PcudaWorker() {
    this->threadRunning = false;
    this->guard = enif_mutex_create((char *) "pcuda_worker");
    this->flag = enif_cond_create((char *) "pcuda_worker");
    this->resultFlag = enif_cond_create((char *) "pcuda_worker");
    this->env = NULL;
    this->buffer = NULL;
}

PcudaWorker::~PcudaWorker() {
    if (this->threadRunning) {
        stopThread();
    }
    if (this->guard) {
        enif_mutex_destroy(this->guard);
    }
    if (this->flag) {
        enif_cond_destroy(this->flag);
    }
    if (this->resultFlag) {
        enif_cond_destroy(this->resultFlag);
    }
}

void PcudaWorker::enqueueCommand(PcudaWorkerCommand *cmd) {
    enif_mutex_lock(this->guard);
    this->queue.push_back(cmd);
    enif_cond_signal(this->flag);
    while(!cmd->done) {
        enif_cond_wait(this->resultFlag, this->guard);
    }
    enif_mutex_unlock(this->guard);
}

const bool PcudaWorker::startThread() {
    if (this->threadRunning) {
        return true;
    }
    if (enif_thread_create((char *) "pcuda_worker", &(this->tid), thunk,  static_cast<void *>(this),
                           NULL) == 0) {
        // UGLY UGLY Hack
        usleep(50 * 1000);
        this->threadRunning = true;
        return true;
    }
    else {
        return false;
    }
}

void PcudaWorker::stopThread() {
    if (this->threadRunning) {
        PcudaWorkerCommand *cmd = new PcudaWorkerCommand();
        cmd->cmd = STOP_WORKER;
        enqueueCommand(cmd);
        enif_thread_join(this->tid, NULL);
        this->threadRunning = false;
    }
}

void *PcudaWorker::thunk(void *args) {
    PcudaWorker *worker = static_cast<PcudaWorker *>(args);
    return worker->loop();
}

void *PcudaWorker::loop() {
    bool keepRunning = true;
    if (setupEnvironment()) {
        enif_mutex_lock(this->guard);
        while (keepRunning) {
            enif_cond_wait(this->flag, this->guard);
            if (this->queue.size() == 0) {
                continue;
            }
            PcudaWorkerCommand *cmd = this->queue.front();
            this->queue.pop_front();
            keepRunning = handleCommand(cmd);
            cmd->done = true;
            enif_cond_signal(this->resultFlag);
        }
        enif_mutex_unlock(this->guard);
        teardownEnvironment();
    }
    return NULL;
}

const bool PcudaWorker::setupEnvironment() {
    this->env = enif_alloc_env();
    return true;
}

void PcudaWorker::teardownEnvironment() {
    enif_free_env(this->env);
    cudaThreadExit();
}

const bool PcudaWorker::handleCommand(PcudaWorkerCommand *cmd) {
    switch(cmd->cmd) {
    case STOP_WORKER:
        return handleShutdown(cmd);
    case NEW_INT_BUFFER:
        return createIntBuffer(cmd);
    case DESTROY_BUFFER:
        return destroyBuffer(cmd);
    case APPEND_BUFFER:
        return appendBuffer(cmd);
    case GET_BUFFER_SIZE:
        return getBufferSize(cmd);
    default:
        return true;
    }
}

const bool PcudaWorker::createIntBuffer(PcudaWorkerCommand *cmd) {
    if (this->buffer == NULL) {
        this->buffer = new PcudaLongBuffer();
        cmd->result = (void *) true;
    }
    else {
        cmd->result = (void *) false;
    }
    return true;
}

const bool PcudaWorker::destroyBuffer(PcudaWorkerCommand *cmd) {
    if (this->buffer != NULL) {
        delete this->buffer;
        this->buffer = NULL;
        cmd->result = (void *) true;
    }
    else {
        cmd->result = (void *) false;
    }
    return true;
}

const bool PcudaWorker::appendBuffer(PcudaWorkerCommand *cmd) {
    if (this->buffer == NULL) {
        cmd->result = (void *) false;
    }
    else {
        std::vector<long> *values = (std::vector<long> *) cmd->arg;
        this->buffer->write(values);
        cmd->result = (void *) true;
    }
    return true;
}

const bool PcudaWorker::getBufferSize(PcudaWorkerCommand *cmd) {
    cmd->result = (void *) this->buffer->size();
    cmd->freeResult = false;
    return true;
}

const bool PcudaWorker::handleShutdown(PcudaWorkerCommand *cmd) {
    return false;
}
