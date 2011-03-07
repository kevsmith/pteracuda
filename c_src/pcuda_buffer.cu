#include <stdio.h>

#include "pcuda_buffer.h"

PcudaLongBuffer::PcudaLongBuffer() {
    this->data = new std::vector<long>();
}

PcudaLongBuffer::~PcudaLongBuffer() {
    delete this->data;
}

void PcudaLongBuffer::write(std::vector<long> *data) {
    this->data->insert(this->data->end(), data->begin(), data->end());
}

long PcudaLongBuffer::size() {
    return this->data->size();
}

void PcudaLongBuffer::clear() {
    this->data->clear();
}
