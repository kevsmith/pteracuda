#include <thrust/device_vector.h>

#include "pteracuda_buffer.h"

int pcuda_new_buffer(pcuda_buffer *buffer, pcuda_types desired_type) {
    int retval = 1;
    switch(desired_type) {
    case PCUDA_TYPE_INT:
        buffer->data = (void *) new thrust::device_vector<int>();
        break;
    case PCUDA_TYPE_LONG:
        buffer->data = (void *) new thrust::device_vector<long>();
        break;
    default:
        buffer->data = NULL;
        retval = 0;
    }
    if (retval) {
        buffer->data_type = desired_type;
    }
    return retval;
}

void pcuda_destroy_buffer(pcuda_buffer *buffer) {
    if (buffer->data != NULL) {
        switch(buffer->data_type) {
        case PCUDA_TYPE_INT:
            delete (thrust::device_vector<int> *) buffer->data;
            break;
        case PCUDA_TYPE_LONG:
            delete (thrust::device_vector<long> *) buffer->data;
            break;
        default:
            break;
        }
    }
}
