#ifndef PCUDA_BUFFER
#define PCUDA_BUFFER

enum pcuda_types {
    PCUDA_TYPE_INT,
    PCUDA_TYPE_LONG,
    PCUDA_TYPE_FLOAT
};

struct pcuda_buffer {
    pcuda_types data_type;
    void *data;
};

int pcuda_new_buffer(pcuda_buffer *buffer, pcuda_types desired_type);
void pcuda_destroy_buffer(pcuda_buffer *buffer);

#endif
