#ifndef PCUDA_OPS
#define PCUDA_OPS

#include <vector>
#include <string>

bool pcuda_integer_sort(std::vector<long> *data);
bool pcuda_integer_binary_search(std::vector<long> *data, long target);
void pcuda_integer_intersection(std::vector<long> *first, std::vector<long> *second, std::vector<long> *intersection);
void pcuda_integer_minmax(std::vector<long> *data, long *minmax);

bool pcuda_float_sort(std::vector<double> *data);
bool pcuda_float_binary_search(std::vector<double> *data, double target);
void pcuda_float_intersection(std::vector<double> *first, std::vector<double> *second, std::vector<double> *intersection);
void pcuda_float_minmax(std::vector<double> *data, double *minmax);

// Work in progress
bool pcuda_string_sort(std::vector<std::string> *data);
#endif
