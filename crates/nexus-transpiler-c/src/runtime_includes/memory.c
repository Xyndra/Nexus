
#include "nexus_core.h"

// ============================================================================
// Memory Management
// ============================================================================

void* nx_malloc(size_t size) {
    void* ptr = malloc(size);
    if (ptr == NULL && size > 0) {
        nx_panic("Memory allocation failed");
    }
    return ptr;
}

void* nx_realloc(void* ptr, size_t size) {
    void* new_ptr = realloc(ptr, size);
    if (new_ptr == NULL && size > 0) {
        nx_panic("Memory reallocation failed");
    }
    return new_ptr;
}

void nx_free(void* ptr) {
    free(ptr);
}
