#include "nexus_core.h"

// ============================================================================
// Std Builtin Functions (always available, no import needed)
// These are wrapper functions that work with value types instead of pointers
// ============================================================================

// Parse a string to i64
int64_t nx_builtin_parse_i64(nx_string s) {
    char* cstr = nx_string_to_cstr(&s);
    int64_t result = strtoll(cstr, NULL, 10);
    nx_free(cstr);
    return result;
}

// Split a string by delimiter
nx_array nx_builtin_split(nx_string s, nx_string delimiter) {
    nx_array result = nx_array_new(sizeof(nx_string));
    
    if (s.len == 0) {
        return result;
    }
    
    // Simple implementation: find delimiter and split
    char* cstr = nx_string_to_cstr(&s);
    char* delim_cstr = nx_string_to_cstr(&delimiter);
    size_t delim_len = strlen(delim_cstr);
    
    if (delim_len == 0) {
        // Empty delimiter: return each character as separate string
        for (size_t i = 0; i < s.len; i++) {
            char single[2] = { (char)(s.data[i] & 0xFF), '\0' };
            nx_string part = nx_string_from_cstr(single);
            nx_array_push(&result, &part);
        }
        nx_free(cstr);
        nx_free(delim_cstr);
        return result;
    }
    
    char* start = cstr;
    char* found;
    
    while ((found = strstr(start, delim_cstr)) != NULL) {
        // Create substring from start to found
        size_t part_len = found - start;
        char* part_cstr = (char*)nx_malloc(part_len + 1);
        #ifdef _WIN32
            memcpy_s(part_cstr, part_len + 1, start, part_len);
        #else
            memcpy(part_cstr, start, part_len);
        #endif
        part_cstr[part_len] = '\0';
        
        nx_string part = nx_string_from_cstr(part_cstr);
        nx_array_push(&result, &part);
        nx_free(part_cstr);
        
        start = found + delim_len;
    }
    
    // Add remaining part
    nx_string remaining = nx_string_from_cstr(start);
    nx_array_push(&result, &remaining);
    
    nx_free(cstr);
    nx_free(delim_cstr);
    
    return result;
}

// Trim whitespace from string
nx_string nx_builtin_trim(nx_string s) {
    if (s.len == 0) {
        return nx_string_from_cstr("");
    }
    
    size_t start = 0;
    size_t end = s.len;
    
    // Find first non-whitespace
    while (start < s.len) {
        uint32_t c = s.data[start];
        if (c != ' ' && c != '\t' && c != '\n' && c != '\r') {
            break;
        }
        start++;
    }
    
    // Find last non-whitespace
    while (end > start) {
        uint32_t c = s.data[end - 1];
        if (c != ' ' && c != '\t' && c != '\n' && c != '\r') {
            break;
        }
        end--;
    }
    
    // Create result
    size_t new_len = end - start;
    nx_string result;
    result.len = new_len;
    result.cap = new_len + 1;
    result.data = (uint32_t*)nx_malloc(sizeof(uint32_t) * result.cap);
    
    #ifdef _WIN32
        memcpy_s(result.data, sizeof(uint32_t) * result.cap, s.data + start, sizeof(uint32_t) * new_len);
    #else
        memcpy(result.data, s.data + start, sizeof(uint32_t) * new_len);
    #endif
    
    return result;
}

// Check if string starts with prefix
bool nx_builtin_starts_with(nx_string s, nx_string prefix) {
    if (prefix.len > s.len) {
        return false;
    }
    
    for (size_t i = 0; i < prefix.len; i++) {
        if (s.data[i] != prefix.data[i]) {
            return false;
        }
    }
    
    return true;
}

// Check if string ends with suffix
bool nx_builtin_ends_with(nx_string s, nx_string suffix) {
    if (suffix.len > s.len) {
        return false;
    }
    
    size_t offset = s.len - suffix.len;
    for (size_t i = 0; i < suffix.len; i++) {
        if (s.data[offset + i] != suffix.data[i]) {
            return false;
        }
    }
    
    return true;
}

// Check if string is empty
bool nx_builtin_is_empty(nx_string s) {
    return s.len == 0;
}

// Join array of strings with delimiter
nx_string nx_builtin_join(nx_array arr, nx_string delimiter) {
    if (arr.len == 0) {
        return nx_string_from_cstr("");
    }
    
    nx_string* strings = (nx_string*)arr.data;
    nx_string result = strings[0];
    
    // Make a copy of the first string
    nx_string first;
    first.len = result.len;
    first.cap = result.len + 1;
    first.data = (uint32_t*)nx_malloc(sizeof(uint32_t) * first.cap);
    #ifdef _WIN32
        memcpy_s(first.data, sizeof(uint32_t) * first.cap, result.data, sizeof(uint32_t) * result.len);
    #else
        memcpy(first.data, result.data, sizeof(uint32_t) * result.len);
    #endif
    result = first;
    
    for (size_t i = 1; i < arr.len; i++) {
        // Append delimiter
        nx_string temp = result;
        result = nx_string_concat(&temp, &delimiter);
        nx_string_free(&temp);
        
        // Append next string
        temp = result;
        result = nx_string_concat(&temp, &strings[i]);
        nx_string_free(&temp);
    }
    
    return result;
}

// Compare two strings for equality
bool nx_builtin_eqs(nx_string a, nx_string b) {
    if (a.len != b.len) {
        return false;
    }
    
    for (size_t i = 0; i < a.len; i++) {
        if (a.data[i] != b.data[i]) {
            return false;
        }
    }
    
    return true;
}

// Get command line arguments (wrapper for nx_compat_args)
nx_array nx_getargs(void) {
    return nx_compat_args();
}

// Read file contents (wrapper that takes nx_string instead of const char*)
nx_string nx_read_file(nx_string path) {
    char* cpath = nx_string_to_cstr(&path);
    nx_string result = nx_compat_read_file(cpath);
    nx_free(cpath);
    return result;
}