#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

struct Complex {
    uint64_t capacity;
    uint64_t size;
    uint64_t data[];
};

uint64_t __nlyapas_rt__get_time() {
    return time(0);
}

uint64_t __nlyapas_rt__rand() {
    return rand();
}

void __nlyapas_rt__seed_rand(uint64_t seed) {
    srand(seed);
}

struct Complex* __nlyapas_rt__alloc_complex(uint64_t size) {
    struct Complex* r = malloc(sizeof(struct Complex) + size*8);
    r->capacity = size;
    r->size = 0;
    return r;
}

void __nlyapas_rt__free_complex(struct Complex* p) {
    free(p);
}

struct Complex* __nlyapas_rt__realloc_complex(struct Complex* old, uint64_t size) {
    if (size <= old->capacity) {
        return old;
    } else {
        struct Complex* new = __nlyapas_rt__alloc_complex(size);
        __nlyapas_rt__free_complex(old);
        return new;
    }
}

void __nlyapas_rt__print_complex(struct Complex* p) {
    fwrite(p->data, 1, p->size, stdout);
}

void __nlyapas_rt__print_string(char* p, uint64_t size) {
    fwrite(p, 1, size, stdout);
}

void __nlyapas_rt__read_complex(struct Complex* p) {
    size_t r = fread(p->data, 1, p->capacity, stdin);
    p->size = r;
}

uint64_t __nlyapas_rt__count_trailing_zeroes(uint64_t x) {
    if (x == 0) {
        return 64;
    }
    uint64_t r = 0;
    while (x != 0) {
        x >>= 1;
        r += 1;
    }
    return r;
}
