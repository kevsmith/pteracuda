#include <string>
#include <vector>

#include <string.h>

#include "erl_nif.h"

#include "pcuda_buffer.h"
#include "pcuda_ops.h"

PCudaStringBuffer::PCudaStringBuffer() {
    this->data = new std::vector<std::string>();
}

PCudaStringBuffer::~PCudaStringBuffer() {
    delete this->data;
}

unsigned int PCudaStringBuffer::size() {
    return this->data->size();
}

bool PCudaStringBuffer::sort() {
    return pcuda_string_sort(this->data);
}

bool PCudaStringBuffer::contains(ErlNifEnv *env, ERL_NIF_TERM rawTarget) {
    return false;
}

ERL_NIF_TERM PCudaStringBuffer::toErlTerms(ErlNifEnv *env) {
    ErlNifBinary bin;
    std::vector<std::string>::iterator iter;
    ERL_NIF_TERM retval = enif_make_list(env, 0);
    if (this->data->size() > 0) {
        for (iter = this->data->end(); iter != this->data->begin();) {
            --iter;
            std::string s = *iter;
            if (enif_alloc_binary(s.size(), &bin)) {
                memcpy(bin.data, s.data(), bin.size);
                retval = enif_make_list_cell(env, enif_make_binary(env, &bin), retval);
            }
        }
    }
    return retval;
}

void PCudaStringBuffer::write(ErlNifEnv *env, ERL_NIF_TERM data) {
    ERL_NIF_TERM head;
    ErlNifBinary bin;

    while (enif_get_list_cell(env, data, &head, &data)) {
        if (enif_inspect_binary(env, head, &bin)) {
            std::string s((char *) bin.data, bin.size);
            this->data->push_back(s);
        }
    }
}

void PCudaStringBuffer::delete_at(unsigned long position) {
    std::vector<std::string>::iterator iter = this->data->begin();
    for (unsigned long i = 0; i < position; i++) {
        iter++;
    }
    this->data->erase(iter);
}

bool PCudaStringBuffer::insert_at(unsigned long position, ErlNifEnv *env, ERL_NIF_TERM value) {
    ErlNifBinary bin;
    if (enif_inspect_binary(env, value, &bin)) {
        std::vector<std::string>::iterator iter = this->data->begin();
        for (unsigned long i = 0; i < position; i++) {
            iter++;
        }
        this->data->insert(iter, 1, std::string((char *) bin.data, bin.size));
        return true;
    }
    return false;
}

void PCudaStringBuffer::clear() {
    this->data->clear();
}

bool PCudaStringBuffer::copy(PCudaBuffer *src) {
    if (src->type() == BUF_TYPE_STRING) {
        PCudaStringBuffer *source = (PCudaStringBuffer *) src;
        std::vector<std::string>::iterator iter;
        for (iter = source->data->begin(); iter != source->data->end(); ++iter) {
            this->data->push_back(*iter);
        }
        return true;
    }
    return false;
}

ERL_NIF_TERM PCudaStringBuffer::intersect(ErlNifEnv *env, PCudaBuffer *other) {
    return enif_make_list(env, 0);
}
