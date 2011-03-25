#include <stdio.h>
#include "pcuda_buffer.h"
#include "pcuda_ops.h"

PCudaFloatBuffer::PCudaFloatBuffer() {
    this->data = new std::vector<double>();
}

PCudaFloatBuffer::~PCudaFloatBuffer() {
    delete this->data;
}

unsigned int PCudaFloatBuffer::size() {
    return this->data->size();
}

void PCudaFloatBuffer::write(ErlNifEnv *env, ERL_NIF_TERM data) {
    ERL_NIF_TERM head;
    double value;

    while (enif_get_list_cell(env, data, &head, &data)) {
        if (enif_get_double(env, head, &value)) {
            this->data->push_back(value);
        }
    }
}

void PCudaFloatBuffer::delete_at(unsigned long position) {
    std::vector<double>::iterator iter = this->data->begin();
    for (unsigned long i = 0; i < position; i++) {
        iter++;
    }
    this->data->erase(iter);
}

bool PCudaFloatBuffer::insert_at(unsigned long position, ErlNifEnv *env, ERL_NIF_TERM rawValue) {
    double value;
    if (enif_get_double(env, rawValue, &value)) {
        std::vector<double>::iterator iter = this->data->begin();
        for (unsigned long i = 0; i < position; i++) {
            iter++;
        }
        this->data->insert(iter, 1, value);
        return true;
    }
    return false;
}

bool PCudaFloatBuffer::sort() {
    return pcuda_float_sort(this->data);
}

bool PCudaFloatBuffer::contains(ErlNifEnv *env, ERL_NIF_TERM rawTarget) {
    double target;
    if (enif_get_double(env, rawTarget, &target)) {
        return pcuda_float_binary_search(this->data, target);
    }
    else {
        return false;
    }
}

ERL_NIF_TERM PCudaFloatBuffer::toErlTerms(ErlNifEnv *env) {
    std::vector<double>::iterator iter;
    ERL_NIF_TERM retval = enif_make_list(env, 0);
    if (this->data->size() > 0) {
        for (iter = this->data->end(); iter != this->data->begin();) {
            --iter;
            retval = enif_make_list_cell(env, enif_make_double(env, *iter), retval);
        }
    }
    return retval;
}

void PCudaFloatBuffer::clear() {
    this->data->clear();
}

bool PCudaFloatBuffer::copy(PCudaBuffer *src) {
    if (src->type() == BUF_TYPE_FLOAT) {
        PCudaFloatBuffer *source = (PCudaFloatBuffer *) src;
        std::vector<double>::iterator iter;
        for (iter = source->data->begin(); iter != source->data->end(); ++iter) {
            this->data->push_back(*iter);
        }
        return true;
    }
    return false;
}

ERL_NIF_TERM PCudaFloatBuffer::intersect(ErlNifEnv *env, PCudaBuffer *otherBuffer) {
    ERL_NIF_TERM retval = enif_make_list(env, 0);
    std::vector<double> intersection;
    if (otherBuffer->type() == BUF_TYPE_FLOAT) {
        PCudaFloatBuffer *other = (PCudaFloatBuffer *) otherBuffer;
        pcuda_float_intersection(this->data, other->data, &intersection);
        if (intersection.size() > 0) {
            for (std::vector<double>::iterator iter = intersection.end(); iter != intersection.begin();) {
                --iter;
                retval = enif_make_list_cell(env, enif_make_double(env, *iter), retval);
            }
        }
    }
    return retval;
}

ERL_NIF_TERM PCudaFloatBuffer::minmax(ErlNifEnv *env) {
    double minmax[2];
    pcuda_float_minmax(this->data, &minmax[0]);
    return enif_make_tuple2(env, enif_make_long(env, minmax[0]), enif_make_long(env, minmax[1]));
}
