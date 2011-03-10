#ifndef PCUDA_OPS
#define PCUDA_OPS

#include <vector>

bool pcuda_integer_sort(std::vector<long> *data);
bool pcuda_integer_binary_search(std::vector<long> *data, long target);
void pcuda_integer_intersection(std::vector<long> *first, std::vector<long> *second, std::vector<long> *intersection);

#endif
