
#include "nexus_core.h"
#include <time.h>

// ============================================================================
// Dynamic Arrays
// ============================================================================

nx_array nx_array_new(size_t elem_size) {
    return nx_array_with_capacity(elem_size, 8);
}

nx_array nx_array_with_capacity(size_t elem_size, size_t capacity) {
    nx_array arr;
    arr.data = capacity > 0 ? nx_malloc(elem_size * capacity) : NULL;
    arr.len = 0;
    arr.cap = capacity;
    arr.elem_size = elem_size;
    return arr;
}

void nx_array_push(nx_array* arr, const void* elem) {
    if (arr->len >= arr->cap) {
        size_t new_cap = arr->cap == 0 ? 8 : arr->cap * 2;
        arr->data = nx_realloc(arr->data, arr->elem_size * new_cap);
        arr->cap = new_cap;
    }
    #ifdef _WIN32
        memcpy_s((char*)arr->data + (arr->len * arr->elem_size), arr->elem_size, elem, arr->elem_size);
    #else
        memcpy((char*)arr->data + (arr->len * arr->elem_size), elem, arr->elem_size);
    #endif
    arr->len++;
}

// Push with explicit element size - reinitializes array elem_size on first push if needed
void nx_array_push_sized(nx_array* arr, const void* elem, size_t elem_size) {
    // If array is empty and was created with wrong elem_size, fix it now
    if (arr->len == 0 && arr->elem_size != elem_size) {
        arr->elem_size = elem_size;
        // Reallocate with correct element size if we have capacity
        if (arr->cap > 0) {
            arr->data = nx_realloc(arr->data, elem_size * arr->cap);
        }
    }
    
    if (arr->len >= arr->cap) {
        size_t new_cap = arr->cap == 0 ? 8 : arr->cap * 2;
        arr->data = nx_realloc(arr->data, arr->elem_size * new_cap);
        arr->cap = new_cap;
    }
    #ifdef _WIN32
        memcpy_s((char*)arr->data + (arr->len * arr->elem_size), arr->elem_size, elem, arr->elem_size);
    #else
        memcpy((char*)arr->data + (arr->len * arr->elem_size), elem, arr->elem_size);
    #endif
    arr->len++;
}

bool nx_array_pop(nx_array* arr, void* out_elem) {
    if (arr->len == 0) {
        return false;
    }
    arr->len--;
    if (out_elem != NULL) {
        #ifdef _WIN32
            memcpy_s(out_elem, arr->elem_size, (char*)arr->data + (arr->len * arr->elem_size), arr->elem_size);
        #else
            memcpy(out_elem, (char*)arr->data + (arr->len * arr->elem_size), arr->elem_size);
        #endif
    }
    return true;
}

void* nx_array_get_checked(nx_array* arr, size_t index) {
    nx_bounds_check(index, arr->len, "array access");
    return (char*)arr->data + (index * arr->elem_size);
}

size_t nx_array_len(nx_array* arr) {
    return arr->len;
}

void nx_array_free(nx_array* arr) {
    if (arr->data != NULL) {
        nx_free(arr->data);
        arr->data = NULL;
    }
    arr->len = 0;
    arr->cap = 0;
}

nx_array nx_array_from_literal(const void* data, size_t len, size_t elem_size) {
    nx_array arr = nx_array_with_capacity(elem_size, len);
    arr.len = len;
    #ifdef _WIN32
        memcpy_s(arr.data, len * elem_size, data, len * elem_size);
    #else
        memcpy(arr.data, data, len * elem_size);
    #endif
    return arr;
}

// Concatenate two arrays into a new array
nx_array nx_array_concat(nx_array* a, nx_array* b) {
    size_t elem_size = a->elem_size > 0 ? a->elem_size : b->elem_size;
    size_t total_len = a->len + b->len;
    nx_array result = nx_array_with_capacity(elem_size, total_len);
    result.len = total_len;
    result.elem_size = elem_size;
    
    if (a->len > 0) {
        #ifdef _WIN32
            memcpy_s(result.data, a->len * elem_size, a->data, a->len * elem_size);
        #else
            memcpy(result.data, a->data, a->len * elem_size);
        #endif
    }
    if (b->len > 0) {
        #ifdef _WIN32
            memcpy_s((char*)result.data + (a->len * elem_size), b->len * elem_size, b->data, b->len * elem_size);
        #else
            memcpy((char*)result.data + (a->len * elem_size), b->data, b->len * elem_size);
        #endif
    }
    return result;
}

// Slice an array from start to end (exclusive) into a new array
nx_array nx_array_slice(nx_array* arr, size_t start, size_t end) {
    // Clamp indices to valid range
    if (start > arr->len) start = arr->len;
    if (end > arr->len) end = arr->len;
    if (start > end) start = end;
    
    size_t slice_len = end - start;
    nx_array result = nx_array_with_capacity(arr->elem_size, slice_len);
    result.len = slice_len;
    result.elem_size = arr->elem_size;
    
    if (slice_len > 0) {
        #ifdef _WIN32
            memcpy_s(result.data, slice_len * arr->elem_size, 
                     (char*)arr->data + (start * arr->elem_size), slice_len * arr->elem_size);
        #else
            memcpy(result.data, (char*)arr->data + (start * arr->elem_size), slice_len * arr->elem_size);
        #endif
    }
    return result;
}
