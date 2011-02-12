#include <unistd.h>
#include "cuda_runtime_api.h"
#include "pteracuda_worker.h"

int handle_command(pcuda_command *command) {
    return 0;
}

void *pcuda_worker_loop(void *args) {
    pcuda_worker *worker = (pcuda_worker *) args;
    int keep_running = 1;
    enif_mutex_lock(worker->command_guard);
    while(keep_running) {
        enif_cond_wait(worker->command_flag, worker->command_guard);
        if (worker->queue->size() > 0) {
            pcuda_command *cmd = worker->queue->front();
            keep_running = handle_command(cmd);
            free(cmd);
        }
    }
    enif_mutex_unlock(worker->command_guard);
    cudaThreadExit();
    return NULL;
}

int pcuda_create_worker(pcuda_worker *worker) {
    int retval = 0;
    if ((worker->command_flag = enif_cond_create((char *) "pcuda_worker")) != NULL &&
        (worker->command_guard = enif_mutex_create((char *) "pcuda_worker")) != NULL) {
        worker->queue = new std::deque<pcuda_command *>();
        if (enif_thread_create((char *) "pcuda_worker", &(worker->tid), pcuda_worker_loop, worker, NULL) == 0) {
            /* UGLY UGLY Hack */
            usleep(50 * 1000);
            worker->running = 1;
            retval = 1;
        }
        else {
            retval = 0;
        }
    }
    return retval;
}

void pcuda_destroy_worker(pcuda_worker *worker) {
    if (worker->running) {
        pcuda_command *cmd = (pcuda_command *) malloc(sizeof(pcuda_command));
        pcuda_enqueue_command(worker, cmd);
        enif_thread_join(worker->tid, NULL);
    }
    if (worker->command_guard != NULL) {
        enif_mutex_destroy(worker->command_guard);
    }
    if (worker->command_flag != NULL) {
        enif_cond_destroy(worker->command_flag);
    }
    if (worker->queue != NULL) {
        delete worker->queue;
    }
}

void pcuda_enqueue_command(pcuda_worker *worker, pcuda_command *cmd) {
    enif_mutex_lock(worker->command_guard);
    worker->queue->push_front(cmd);
    enif_mutex_unlock(worker->command_guard);
    enif_cond_signal(worker->command_flag);
}
