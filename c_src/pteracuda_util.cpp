#include "erl_nif.h"
#include "pteracuda_util.h"

void pcuda_maybe_free(void *thing) {
    if (thing) {
        enif_free(thing);
    }
}
