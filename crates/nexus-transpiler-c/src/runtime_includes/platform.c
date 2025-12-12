
#include "nexus_core.h"

// ============================================================================
// Platform-Specific Functions
// ============================================================================

int32_t nx_plat_system(const char* command) {
    return (int32_t)system(command);
}

const char* nx_plat_name(void) {
    return PLATFORM_NAME;
}

const char* nx_plat_arch(void) {
    return PLATFORM_ARCH;
}
