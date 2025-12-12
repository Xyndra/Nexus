
#include "nexus_core.h"

// ============================================================================
// Error Handling
// ============================================================================

void nx_panic(const char* message) {
    fprintf(stderr, "PANIC: %s\n", message);
    exit(1);
}

void nx_bounds_check(size_t index, size_t len, const char* context) {
    if (index >= len) {
        fprintf(stderr, "Bounds check failed in %s: index %zu >= length %zu\n",
                context, index, len);
        exit(1);
    }
}
