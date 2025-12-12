
#include "nexus_core.h"

// ============================================================================
// Error Handling (Bounds Checking Disabled)
// ============================================================================

void nx_panic(const char* message) {
    fprintf(stderr, "PANIC: %s\n", message);
    exit(1);
}

void nx_bounds_check(size_t index, size_t len, const char* context) {
    // Bounds checking disabled
    (void)index;
    (void)len;
    (void)context;
}
