
#include "nexus_core.h"

// ============================================================================
// Strings
// ============================================================================

nx_string nx_string_from_cstr(const char* cstr) {
    nx_string str;
    size_t len = strlen(cstr);
    str.len = len;
    str.cap = len + 1;
    str.data = (uint32_t*)nx_malloc(sizeof(uint32_t) * str.cap);

    // Simple ASCII conversion (should be UTF-8 in production)
    for (size_t i = 0; i < len; i++) {
        str.data[i] = (uint32_t)(unsigned char)cstr[i];
    }

    return str;
}

char* nx_string_to_cstr(nx_string* str) {
    char* cstr = (char*)nx_malloc(str->len + 1);

    // Simple conversion (should handle UTF-8 properly in production)
    for (size_t i = 0; i < str->len; i++) {
        cstr[i] = (char)(str->data[i] & 0xFF);
    }
    cstr[str->len] = '\0';

    return cstr;
}

size_t nx_string_len(nx_string* str) {
    return str->len;
}

void nx_string_free(nx_string* str) {
    if (str->data != NULL) {
        nx_free(str->data);
        str->data = NULL;
    }
    str->len = 0;
    str->cap = 0;
}

nx_string nx_string_concat(nx_string* a, nx_string* b) {
    nx_string result;
    result.len = a->len + b->len;
    result.cap = result.len + 1;
    result.data = (uint32_t*)nx_malloc(sizeof(uint32_t) * result.cap);

    #ifdef _WIN32
        memcpy_s(result.data, sizeof(uint32_t) * result.cap, a->data, sizeof(uint32_t) * a->len);
        memcpy_s(result.data + a->len, sizeof(uint32_t) * (result.cap - a->len), b->data, sizeof(uint32_t) * b->len);
    #else
        memcpy(result.data, a->data, sizeof(uint32_t) * a->len);
        memcpy(result.data + a->len, b->data, sizeof(uint32_t) * b->len);
    #endif

    return result;
}

// Forward declaration for recursive array handling
static nx_string nx_string_from_array_recursive(nx_array* arr, int depth);

nx_string nx_string_from_value(nx_value val) {
    char buffer[64];

    switch (val.tag) {
        case NX_TYPE_I64:
            snprintf(buffer, sizeof(buffer), "%lld", (long long)val.data.as_i64);
            return nx_string_from_cstr(buffer);

        case NX_TYPE_I32:
            snprintf(buffer, sizeof(buffer), "%d", (int)val.data.as_i32);
            return nx_string_from_cstr(buffer);

        case NX_TYPE_U64:
            snprintf(buffer, sizeof(buffer), "%llu", (unsigned long long)val.data.as_u64);
            return nx_string_from_cstr(buffer);

        case NX_TYPE_U32:
            snprintf(buffer, sizeof(buffer), "%u", (unsigned int)val.data.as_u32);
            return nx_string_from_cstr(buffer);

        case NX_TYPE_F64:
            snprintf(buffer, sizeof(buffer), "%g", val.data.as_f64);
            return nx_string_from_cstr(buffer);

        case NX_TYPE_F32:
            snprintf(buffer, sizeof(buffer), "%g", (double)val.data.as_f32);
            return nx_string_from_cstr(buffer);

        case NX_TYPE_BOOL:
            return nx_string_from_cstr(val.data.as_bool ? "true" : "false");

        case NX_TYPE_STRING:
            // Return a copy of the string
            if (val.data.as_string != NULL) {
                return *val.data.as_string;
            }
            return nx_string_from_cstr("");

        case NX_TYPE_ARRAY:
            if (val.data.as_array != NULL) {
                return nx_string_from_array_recursive(val.data.as_array, 0);
            }
            return nx_string_from_cstr("[]");

        default:
            return nx_string_from_cstr("[unknown]");
    }
}

// Helper function to recursively convert arrays to strings
static nx_string nx_string_from_array_recursive(nx_array* arr, int depth) {
    // Prevent infinite recursion
    if (depth > 10) {
        return nx_string_from_cstr("[...]");
    }

    if (arr->len == 0) {
        return nx_string_from_cstr("[]");
    }

    // Build array string by converting each element
    nx_string result = nx_string_from_cstr("[");

    for (size_t i = 0; i < arr->len; i++) {
        if (i > 0) {
            nx_string comma = nx_string_from_cstr(", ");
            nx_string temp = result;
            result = nx_string_concat(&temp, &comma);
            nx_string_free(&temp);
            nx_string_free(&comma);
        }

        // Convert element based on element size (heuristic)
        nx_value elem_val;
        elem_val.tag = NX_TYPE_UNKNOWN;
        void* elem_ptr = (char*)arr->data + (i * arr->elem_size);

        if (arr->elem_size == sizeof(nx_array)) {
            // Nested array
            nx_array* nested_arr = (nx_array*)elem_ptr;
            elem_val.tag = NX_TYPE_ARRAY;
            elem_val.data.as_array = nested_arr;
        } else if (arr->elem_size == sizeof(nx_string)) {
            // Array of strings
            nx_string* str_ptr = (nx_string*)elem_ptr;
            elem_val.tag = NX_TYPE_STRING;
            elem_val.data.as_string = str_ptr;
        } else if (arr->elem_size == sizeof(int64_t)) {
            // Assume int64_t
            int64_t* data_i64 = (int64_t*)arr->data;
            elem_val.tag = NX_TYPE_I64;
            elem_val.data.as_i64 = data_i64[i];
        } else if (arr->elem_size == sizeof(int32_t)) {
            // Assume int32_t
            int32_t* data_i32 = (int32_t*)arr->data;
            elem_val.tag = NX_TYPE_I32;
            elem_val.data.as_i32 = data_i32[i];
        } else if (arr->elem_size == sizeof(double)) {
            // Could be double - check if it's actually int64_t first
            int64_t* data_i64 = (int64_t*)arr->data;
            elem_val.tag = NX_TYPE_I64;
            elem_val.data.as_i64 = data_i64[i];
        } else if (arr->elem_size == sizeof(float)) {
            // Assume float
            float* data_f32 = (float*)arr->data;
            elem_val.tag = NX_TYPE_F32;
            elem_val.data.as_f32 = data_f32[i];
        } else if (arr->elem_size == sizeof(bool)) {
            // Assume bool
            bool* data_bool = (bool*)arr->data;
            elem_val.tag = NX_TYPE_BOOL;
            elem_val.data.as_bool = data_bool[i];
        } else {
            // Unknown type, just show ?
            char buffer[64];
            snprintf(buffer, sizeof(buffer), "?");
            nx_string elem_str = nx_string_from_cstr(buffer);
            nx_string temp = result;
            result = nx_string_concat(&temp, &elem_str);
            nx_string_free(&temp);
            nx_string_free(&elem_str);
            continue;
        }

        nx_string elem_str;
        if (elem_val.tag == NX_TYPE_ARRAY && elem_val.data.as_array != NULL) {
            // Recursive call for nested arrays
            elem_str = nx_string_from_array_recursive(elem_val.data.as_array, depth + 1);
        } else {
            elem_str = nx_string_from_value(elem_val);
        }

        nx_string temp = result;
        result = nx_string_concat(&temp, &elem_str);
        nx_string_free(&temp);
        nx_string_free(&elem_str);
    }

    nx_string close = nx_string_from_cstr("]");
    nx_string temp = result;
    result = nx_string_concat(&temp, &close);
    nx_string_free(&temp);
    nx_string_free(&close);

    return result;
}
