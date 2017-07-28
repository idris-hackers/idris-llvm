#include <stdint.h>

struct valTy {
    int32_t tag;
    void* val;
};

typedef struct valTy valTy;