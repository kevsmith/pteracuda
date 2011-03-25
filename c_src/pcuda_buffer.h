#ifndef PCUDA_BUFFER
#define PCUDA_BUFFER

#include <string>
#include <vector>

#include "erl_nif.h"

enum PCudaBufferTypes {
    BUF_TYPE_INTEGER,
    BUF_TYPE_STRING,
    BUF_TYPE_FLOAT
};

class PCudaBuffer {
public:
    virtual ~PCudaBuffer() { };
    virtual unsigned int size() = 0;
    virtual PCudaBufferTypes type() = 0;
    virtual bool sort() = 0;
    virtual bool contains(ErlNifEnv *env, ERL_NIF_TERM rawTarget) = 0;
    virtual void write(ErlNifEnv *env, ERL_NIF_TERM data) = 0;
    virtual void delete_at(unsigned long position) = 0;
    virtual bool insert_at(unsigned long position, ErlNifEnv *env, ERL_NIF_TERM value) = 0;
    virtual void clear() = 0;
    virtual bool copy(PCudaBuffer *src) = 0;
    virtual ERL_NIF_TERM intersect(ErlNifEnv *env, PCudaBuffer *other) = 0;
    virtual ERL_NIF_TERM minmax(ErlNifEnv *env) = 0;
    virtual ERL_NIF_TERM toErlTerms(ErlNifEnv *env) = 0;
};

class PCudaIntBuffer : public PCudaBuffer {
public:
    PCudaIntBuffer();
    virtual ~PCudaIntBuffer();
    virtual unsigned int size();
    virtual PCudaBufferTypes type() { return BUF_TYPE_INTEGER; };
    virtual bool sort();
    virtual bool contains(ErlNifEnv *env, ERL_NIF_TERM rawTarget);
    virtual ERL_NIF_TERM toErlTerms(ErlNifEnv *env);
    virtual void write(ErlNifEnv *env, ERL_NIF_TERM data);
    virtual void delete_at(unsigned long position);
    virtual bool insert_at(unsigned long position, ErlNifEnv *env, ERL_NIF_TERM value);
    virtual void clear();
    virtual bool copy(PCudaBuffer *src);
    virtual ERL_NIF_TERM intersect(ErlNifEnv *env, PCudaBuffer *other);
    virtual ERL_NIF_TERM minmax(ErlNifEnv *env);

protected:
    std::vector<long> *data;
};

class PCudaFloatBuffer : public PCudaBuffer {
public:
    PCudaFloatBuffer();
    virtual ~PCudaFloatBuffer();
    virtual unsigned int size();
    virtual PCudaBufferTypes type() { return BUF_TYPE_FLOAT; };
    virtual bool sort();
    virtual bool contains(ErlNifEnv *env, ERL_NIF_TERM rawTarget);
    virtual ERL_NIF_TERM toErlTerms(ErlNifEnv *env);
    virtual void write(ErlNifEnv *env, ERL_NIF_TERM data);
    virtual void delete_at(unsigned long position);
    virtual bool insert_at(unsigned long position, ErlNifEnv *env, ERL_NIF_TERM value);
    virtual void clear();
    virtual bool copy(PCudaBuffer *src);
    virtual ERL_NIF_TERM intersect(ErlNifEnv *env, PCudaBuffer *other);
    virtual ERL_NIF_TERM minmax(ErlNifEnv *env);

protected:
    std::vector<double> *data;
};

class PCudaStringBuffer : public PCudaBuffer {
public:
    PCudaStringBuffer();
    virtual ~PCudaStringBuffer();
    virtual unsigned int size();
    virtual PCudaBufferTypes type() { return BUF_TYPE_STRING; };
    virtual bool sort();
    virtual bool contains(ErlNifEnv *env, ERL_NIF_TERM rawTarget);
    virtual ERL_NIF_TERM toErlTerms(ErlNifEnv *env);
    virtual void write(ErlNifEnv *env, ERL_NIF_TERM data);
    virtual void delete_at(unsigned long position);
    virtual bool insert_at(unsigned long position, ErlNifEnv *env, ERL_NIF_TERM value);
    virtual void clear();
    virtual bool copy(PCudaBuffer *src);
    virtual ERL_NIF_TERM intersect(ErlNifEnv *env, PCudaBuffer *other);
    virtual ERL_NIF_TERM minmax(ErlNifEnv *env) { return enif_make_atom(env, "error"); };

protected:
    std::vector<std::string> *data;
};
#endif
