
#include "nexus_core.h"

// ============================================================================
// Compat Process Functions
// ============================================================================

void nx_compat_exit(int32_t code) {
    exit(code);
}

nx_string nx_compat_getenv(const char* name) {
    const char* value = getenv(name);
    if (value == NULL) {
        return nx_string_from_cstr("");
    }
    return nx_string_from_cstr(value);
}

// Global for storing command line arguments
static int g_argc = 0;
static char** g_argv = NULL;

void nx_init_args(int argc, char** argv) {
    g_argc = argc;
    g_argv = argv;
}

nx_array nx_compat_args(void) {
    nx_array arr = nx_array_with_capacity(sizeof(nx_string), g_argc);

    for (int i = 0; i < g_argc; i++) {
        nx_string arg = nx_string_from_cstr(g_argv[i]);
        nx_array_push(&arr, &arg);
    }

    return arr;
}
