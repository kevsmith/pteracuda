#ifndef PCUDA_BUFFER
#define PCUDA_BUFFER

enum pcuda_types {
    PCUDA_LONG,
    PCUDA_INT,
    PCUDA_FLOAT,
    PCUDA_CHAR
};

typedef struct _pcuda_buffer {
    pcuda_types dtype;
    void *buffer;
} pcuda_buffer;

int pcuda_new_buffer(pcuda_buffer *buffer, pcuda_types buffer_type);
void pcuda_destroy_buffer(pcuda_buffer *buffer);

#endif
