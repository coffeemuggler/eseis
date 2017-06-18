#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP eseis_stalta_event_freeze(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP eseis_stalta_event_nofreeze(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"eseis_stalta_event_freeze",   (DL_FUNC) &eseis_stalta_event_freeze,   5},
    {"eseis_stalta_event_nofreeze", (DL_FUNC) &eseis_stalta_event_nofreeze, 4},
    {NULL, NULL, 0}
};

void R_init_eseis(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
