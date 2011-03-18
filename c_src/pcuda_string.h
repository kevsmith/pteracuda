#ifndef PCUDA_STRING
#define PCUDA_STRING

#include <string>

#include <thrust/device_ptr.h>
#include <thrust/copy.h>

class PCudaString {
public:
    __host__ __device__ PCudaString();
    __host__ PCudaString(const std::string& other);
    __host__ __device__ inline PCudaString(const PCudaString& other);

    __host__ int length();
    __host__ int cstr_length();
    // Explicit destructor as the C++ one gets called
    // while code is running on the CUDA card
    __host__ void destroy();

    __host__ operator std::string();

    // Begrudgingly made these public so on-device
    // sorting would work
    char *str;
    thrust::device_ptr<char> ptr;
    int len;
};

__device__ bool operator< (PCudaString lhs, PCudaString rhs);

#endif
