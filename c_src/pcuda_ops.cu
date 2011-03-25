#include <vector>
#include <stdio.h>

#include "cuda.h"

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/copy.h>
#include <thrust/sort.h>
#include <thrust/functional.h>
#include <thrust/binary_search.h>
#include <thrust/set_operations.h>
#include <thrust/extrema.h>

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

PCudaString::PCudaString(const PCudaString& other) {
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

bool pcuda_integer_sort(std::vector<long> *data) {
    thrust::device_vector<long> device = *data;
    thrust::sort(device.begin(), device.end());
    thrust::copy(device.begin(), device.end(), data->begin());
    return true;
}

bool pcuda_float_sort(std::vector<double> *data) {
    thrust::device_vector<double> device = *data;
    thrust::sort(device.begin(), device.end());
    thrust::copy(device.begin(), device.end(), data->begin());
    return true;
}

bool pcuda_string_sort(std::vector<std::string> *data) {
    printf("In pcuda_string_sort\n");
    thrust::device_vector<PCudaString> device;
    printf("Reserving memory\n");
    device.reserve(data->size());
    printf("Copying data to device\n");
    for (std::vector<std::string>::iterator iter = data->begin();
         iter != data->end(); ++iter) {
        std::string s = *iter;
        device.push_back(s);
    }
    printf("On-device sort\n");
    thrust::sort(device.begin(), device.end());
    printf("Copying data from device\n");
    thrust::host_vector<PCudaString> results = device;
    data->clear();
    for (thrust::host_vector<PCudaString>::iterator iter = results.begin();
         iter != results.end(); ++iter) {
        PCudaString cs = *iter;
        std::string s = cs;
        cs.destroy();
        data->push_back(s);
    }
    printf("Done!\n");
    return true;
}

bool pcuda_integer_binary_search(std::vector<long> *data, long target) {
    thrust::device_vector<long> device = *data;
    return thrust::binary_search(device.begin(), device.end(), target, thrust::less<long>());
}

bool pcuda_float_binary_search(std::vector<double> *data, double target) {
    thrust::device_vector<double> device = *data;
    return thrust::binary_search(device.begin(), device.end(), target, thrust::less<double>());
}

void pcuda_integer_intersection(std::vector<long> *first, std::vector<long> *second,
                                std::vector<long> *intersection) {
    thrust::set_intersection(first->begin(), first->end(),
                             second->begin(), second->end(), std::back_inserter(*intersection));
}

void pcuda_float_intersection(std::vector<double> *first, std::vector<double> *second,
                                std::vector<double> *intersection) {
    thrust::set_intersection(first->begin(), first->end(),
                             second->begin(), second->end(), std::back_inserter(*intersection));
}

void pcuda_integer_minmax(std::vector<long> *data, long *minmax) {
    thrust::pair<std::vector<long>::iterator,
                 std::vector<long>::iterator> result = thrust::minmax_element(data->begin(), data->end());
    minmax[0] = *result.first;
    minmax[1] = *result.second;
}

void pcuda_float_minmax(std::vector<double> *data, double *minmax) {
    thrust::pair<std::vector<double>::iterator,
                 std::vector<double>::iterator> result = thrust::minmax_element(data->begin(), data->end());
    minmax[0] = *result.first;
    minmax[1] = *result.second;
}
