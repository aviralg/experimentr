#include "utilities.h"
#include <string>

SEXP run_length_encoding_helper(const char* element) {
    if (element == NULL) {
        return NA_STRING;
    }

    std::string new_element;

    int len = strlen(element);

    if (len == 0) {
        return mkChar(element);
    }

    int i = 0;
    while (i < len) {
        new_element.push_back(element[i]);
        new_element.push_back('+');
        ++i;
        while (i < len && element[i] == element[i - 1]) {
            ++i;
        }
    }

    return mkChar(new_element.c_str());
}

SEXP r_experimentr_run_length_encoding(SEXP r_input) {
    int length = Rf_length(r_input);
    SEXP r_output = PROTECT(allocVector(STRSXP, length));

    for (int i = 0; i < length; ++i) {
        SEXP r_char = STRING_ELT(r_input, i);
        const char* element = r_char == NA_STRING ? NULL : CHAR(r_char);
        SEXP r_new_char = run_length_encoding_helper(element);
        SET_STRING_ELT(r_output, i, r_new_char);
    }

    UNPROTECT(1);
    return r_output;
}
