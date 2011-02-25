#include <unistd.h>
#include <stdio.h>
#include "cuda_runtime_api.h"
#include "erl_nif.h"
#include "pcuda_worker.h"

PcudaWorker::PcudaWorker() {
    this->threadRunning = false;
    this->guard = enif_mutex_create((char *) "pcuda_worker");
    this->flag = enif_cond_create((char *) "pcuda_worker");
    this->env = NULL;
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
}

const bool PcudaWorker::enqueueCommand(PcudaWorkerCommand *cmd) {
    enif_mutex_lock(this->guard);
    this->queue.push_front(cmd);
    enif_mutex_unlock(this->guard);
    enif_cond_signal(this->flag);
    return true;
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
        cmd->caller = NULL;
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
            while(this->queue.size() > 0) {
                PcudaWorkerCommand *cmd = this->queue.front();
                this->queue.pop_front();
                if (cmd->cmd == STOP_WORKER) {
                    keepRunning = false;
                }
                enif_mutex_unlock(this->guard);
                enif_mutex_lock(this->guard);
            }
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
