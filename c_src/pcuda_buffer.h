#ifndef PCUDA_BUFFER
#define PCUDA_BUFFER

#include <vector>

class PcudaLongBuffer {
public:
    PcudaLongBuffer();
    virtual ~PcudaLongBuffer();
    void write(std::vector<long> *data);
    long size();
    void clear();
private:
    std::vector<long> *data;
};

#endif
