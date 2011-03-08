#ifndef PCUDA_BUFFER
#define PCUDA_BUFFER

#include <thrust/host_vector.h>

class PcudaLongBuffer {
public:
    PcudaLongBuffer();
    virtual ~PcudaLongBuffer();
    void write(std::vector<long> *data);
    void read(std::vector<long> *data);
    void sort();
    long size();
    void clear();
private:
    thrust::host_vector<long> *data;
};

#endif
