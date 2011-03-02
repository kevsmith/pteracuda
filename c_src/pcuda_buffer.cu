#include "pcuda_buffer.h"

PcudaIntBuffer::PcudaIntBuffer() {
    this->data = new thrust::device_vector<int>();
}

PcudaIntBuffer::~PcudaIntBuffer() {
    delete this->data;
}

pcuda_types PcudaIntBuffer::getBufferType() {
    return PCUDA_TYPE_INT;
}
