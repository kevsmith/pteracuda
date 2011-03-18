#include <vector>

#include "cuda.h"

#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/sort.h>

#include "pcuda_string.h"

PCudaString::PCudaString() {
    this->len = -1;
    this->str = NULL;
}

PCudaString::PCudaString(const std::string& other) {
    this->len = other.length();
    this->ptr = thrust::device_malloc(this->len + 1);
    this->str = raw_pointer_cast(this->ptr);
    cudaMemcpy(this->str, other.c_str(), this->len, cudaMemcpyHostToDevice);
}

inline PCudaString::PCudaString(const PCudaString& other) {
    this->len = other.len;
    this->str = other.str;
    this->ptr = other.ptr;
}

int PCudaString::length() {
    return this->len;
}

int PCudaString::cstr_length() {
    return this->len + 1;
}

PCudaString::operator std::string() {
    std::string retval;
    thrust::copy(this->ptr, this->ptr + this->len, back_inserter(retval));
    return retval;
}


void PCudaString::destroy() {
    if (this->str) {
        thrust::device_free(this->ptr);
        this->str = NULL;
        this->len = -1;
    }
}

bool operator< (PCudaString lhs, PCudaString rhs) {
    char *l = lhs.str;
    char *r = rhs.str;
    while((*l && *r) && *l == *r) {
        ++l;
        ++r;
    }
    return *l < *r;
}
