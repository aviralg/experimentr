#include "Rincludes.h"
#include <stdlib.h> // for NULL
#include "utilities.h"

extern "C" {

static const R_CallMethodDef callMethods[] = {
    {"r_experimentr_run_length_encoding", (DL_FUNC) &r_experimentr_run_length_encoding, 1},
    {NULL, NULL, 0}};

void R_init_strictr(DllInfo* dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

}
