#include <vector>

#include <thrust/host_vector.h>
#include <thrust/device_vector.h>
#include <thrust/copy.h>
#include <thrust/sort.h>

bool pcuda_integer_sort(std::vector<long> *data) {
    thrust::device_vector<long> device = *data;
    thrust::sort(device.begin(), device.end());
    thrust::copy(device.begin(), device.end(), data->begin());
    return true;
}
