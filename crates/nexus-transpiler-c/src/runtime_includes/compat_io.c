
#include "nexus_core.h"
#include <time.h>

// ============================================================================
// Compat I/O Functions
// ============================================================================

// Helper function to print a single typed value
static void nx_print_value(nx_value* val) {
    switch (val->tag) {
        case NX_TYPE_I64:
            printf("%lld", (long long)val->data.as_i64);
            break;
        case NX_TYPE_I32:
            printf("%d", (int)val->data.as_i32);
            break;
        case NX_TYPE_U64:
            printf("%llu", (unsigned long long)val->data.as_u64);
            break;
        case NX_TYPE_U32:
            printf("%u", (unsigned int)val->data.as_u32);
            break;
        case NX_TYPE_F64:
            printf("%g", val->data.as_f64);
            break;
        case NX_TYPE_F32:
            printf("%g", (double)val->data.as_f32);
            break;
        case NX_TYPE_BOOL:
            printf("%s", val->data.as_bool ? "true" : "false");
            break;
        case NX_TYPE_STRING:
            if (val->data.as_string != NULL) {
                char* cstr = nx_string_to_cstr(val->data.as_string);
                printf("%s", cstr);
                nx_free(cstr);
            }
            break;
        case NX_TYPE_ARRAY:
            if (val->data.as_array != NULL) {
                nx_string arr_str = nx_string_from_value(*val);
                char* cstr = nx_string_to_cstr(&arr_str);
                printf("%s", cstr);
                nx_free(cstr);
                nx_string_free(&arr_str);
            } else {
                printf("[]");
            }
            break;
        default:
            printf("[unknown]");
            break;
    }
}

void nx_compat_print(nx_value* values, size_t count) {
    for (size_t i = 0; i < count; i++) {
        if (i > 0) {
            printf(" ");
        }
        nx_print_value(&values[i]);
    }
    fflush(stdout);
}

void nx_compat_println(nx_value* values, size_t count) {
    nx_compat_print(values, count);
    printf("\n");
    fflush(stdout);
}

nx_string nx_compat_readln(void) {
    char buffer[4096];
    if (fgets(buffer, sizeof(buffer), stdin) == NULL) {
        return nx_string_from_cstr("");
    }

    // Remove trailing newline
    size_t len = strlen(buffer);
    if (len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
    }

    return nx_string_from_cstr(buffer);
}

nx_string nx_compat_read_file(const char* path) {
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        nx_panic("Failed to open file for reading");
    }

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = (char*)nx_malloc(size + 1);
    size_t read = fread(buffer, 1, size, file);
    buffer[read] = '\0';
    fclose(file);

    nx_string result = nx_string_from_cstr(buffer);
    nx_free(buffer);
    return result;
}

bool nx_compat_write_file(const char* path, const char* content) {
    FILE* file = fopen(path, "wb");
    if (file == NULL) {
        return false;
    }

    size_t len = strlen(content);
    size_t written = fwrite(content, 1, len, file);
    fclose(file);

    return written == len;
}

bool nx_compat_file_exists(const char* path) {
    FILE* file = fopen(path, "r");
    if (file != NULL) {
        fclose(file);
        return true;
    }
    return false;
}
