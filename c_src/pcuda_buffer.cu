#include <stdio.h>

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/sort.h>

#include "pcuda_buffer.h"

PcudaLongBuffer::PcudaLongBuffer() {
    this->data = new thrust::host_vector<long>();
}

PcudaLongBuffer::~PcudaLongBuffer() {
    delete this->data;
}

void PcudaLongBuffer::write(std::vector<long> *data) {
    this->data->insert(this->data->end(), data->begin(), data->end());
}

void PcudaLongBuffer::read(std::vector<long> *data) {
    data->insert(data->begin(), this->data->begin(), this->data->end());
}

void PcudaLongBuffer::sort() {
    thrust::device_vector<long> d_vec = *(this->data);
    thrust::sort(d_vec.begin(), d_vec.end());
    this->data->clear();
    this->data->insert(this->data->end(), d_vec.begin(), d_vec.end());
}

long PcudaLongBuffer::size() {
    return this->data->size();
}

void PcudaLongBuffer::clear() {
    this->data->clear();
}
