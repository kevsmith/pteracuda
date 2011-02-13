#include <thrust/device_vector.h>

#include "pteracuda_buffer.h"

void destroy_device_buffer(pcuda_buffer *buffer) {
    if (buffer->dtype == PCUDA_LONG) {
        thrust::device_vector<long> *data = (thrust::device_vector<long> *) buffer->buffer;
        delete data;
    }
    else if(buffer->dtype == PCUDA_INT) {
        thrust::device_vector<int> *data = (thrust::device_vector<int> *) buffer->buffer;
        delete data;
    }
    else if(buffer->dtype == PCUDA_FLOAT) {
        thrust::device_vector<float> *data = (thrust::device_vector<float> *) buffer->buffer;
        delete data;
    }
    else if(buffer->dtype == PCUDA_CHAR) {
        thrust::device_vector<char> *data = (thrust::device_vector<char> *) buffer->buffer;
        delete data;
    }
}

int pcuda_new_buffer(pcuda_buffer *buffer, pcuda_types buffer_type) {
    return 0;
}

void pcuda_destroy_buffer(pcuda_buffer *buffer) {
}
