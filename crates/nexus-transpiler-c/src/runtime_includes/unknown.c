
#include "nexus_core.h"

// ============================================================================
// Unknown/Sum Types
// ============================================================================

nx_unknown nx_unknown_new(size_t variant_index, const void* value, size_t value_size) {
    nx_unknown u;
    u.variant_index = variant_index;
    u.value_size = value_size;
    u.value = nx_malloc(value_size);
    #ifdef _WIN32
        memcpy_s(u.value, value_size, value, value_size);
    #else
        memcpy(u.value, value, value_size);
    #endif
    return u;
}

size_t nx_unknown_variant(nx_unknown* u) {
    return u->variant_index;
}

void* nx_unknown_value(nx_unknown* u) {
    return u->value;
}

void nx_unknown_free(nx_unknown* u) {
    if (u->value != NULL) {
        nx_free(u->value);
        u->value = NULL;
    }
}
