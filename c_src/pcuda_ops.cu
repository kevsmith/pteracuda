#include <vector>

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/copy.h>
#include <thrust/sort.h>
#include <thrust/functional.h>
#include <thrust/binary_search.h>

bool pcuda_integer_sort(std::vector<long> *data) {
    thrust::device_vector<long> device = *data;
    thrust::sort(device.begin(), device.end());
    thrust::copy(device.begin(), device.end(), data->begin());
    return true;
}

bool pcuda_integer_binary_search(std::vector<long> *data, long target) {
    thrust::device_vector<long> device = *data;
    return thrust::binary_search(device.begin(), device.end(), target, thrust::less<long>());
}
