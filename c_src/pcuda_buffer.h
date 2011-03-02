#ifndef PCUDA_BUFFER
#define PCUDA_BUFFER

#include <thrust/device_vector.h>

enum pcuda_types {
    PCUDA_TYPE_INT,
    PCUDA_TYPE_LONG,
    PCUDA_TYPE_FLOAT
};

/* class PcudaBufferInterface { */
/* public: */
/*     virtual ~PcudaBufferInterface(); */
/*     virtual pcuda_types getBufferType() = 0; */
/* }; */

class PcudaIntBuffer {
public:
    PcudaIntBuffer();
    virtual ~PcudaIntBuffer();
    virtual pcuda_types getBufferType();
private:
    thrust::device_vector<int> *data;
};

#endif
