#include <R.h>
#include <Rinternals.h>

#include "libmseed/libmseed.h"

/* R-callable MiniSEED reader using ms3_readtracelist() */
SEXP parseMiniSEED_v3(SEXP r_path)
{
  /* ---- Validate input ---- */
  if (TYPEOF(r_path) != STRSXP || LENGTH(r_path) != 1)
    Rf_error("parseMiniSEED_v3: path must be a single string");
  
  const char *filename = CHAR(STRING_ELT(r_path, 0));
  
  /* ---- libmseed structures ---- */
  MS3TraceList *mstl = NULL;
  MS3Tolerance tol   = MS3Tolerance_INITIALIZER;
  uint32_t flags     = MSF_UNPACKDATA | MSF_SKIPNOTDATA;
  int verbose        = 0;
  int retcode;
  
  /* ---- Read the file into a trace list ---- */
  retcode = ms3_readtracelist(&mstl, filename, &tol, 0, flags, verbose);
  
  if (retcode != MS_NOERROR)
    Rf_error("parseMiniSEED_v3: cannot read file: %s", ms_errorstr(retcode));
  
  if (!mstl)
    Rf_error("parseMiniSEED_v3: no data found in file");
  
  MS3TraceID *tid;
  MS3TraceSeg *seg;
  
  /* ---- Count number of traces ---- */
  int ntraces = 0;
  for (tid = mstl->traces.next[0]; tid != NULL; tid = tid->next[0])
    ntraces++;
  
  /* ---- Create top-level list ---- */
  SEXP r_traces = PROTECT(allocVector(VECSXP, ntraces));
  
  int trace_index = 0;
  
  /* ---- Iterate over trace IDs ---- */
  for (tid = mstl->traces.next[0]; tid != NULL; tid = tid->next[0])
  {
    const char *sid = tid->sid;  /* e.g. "FDSN:GR.MOX..BHZ" */
  
  /* Count segments */
  int nsegs = 0;
  for (seg = tid->first; seg != NULL; seg = seg->next)
    nsegs++;
  
  /* Allocate segment list */
  SEXP r_seglist = PROTECT(allocVector(VECSXP, nsegs));
  int seg_index = 0;
  
  /* ---- Fill segment list ---- */
  for (seg = tid->first; seg != NULL; seg = seg->next)
  {
    double samprate = seg->samprate;
    int64_t npts    = seg->numsamples;
    double tstart   = seg->starttime;
    double tend     = seg->endtime;
    char sampletype = seg->sampletype;
    
    /* Convert samples to R numeric vector */
    SEXP r_data = PROTECT(allocVector(REALSXP, npts));
    double *rptr = REAL(r_data);
    
    if (sampletype == 'i')
    {
      int32_t *iptr = (int32_t *) seg->datasamples;
      for (int64_t i = 0; i < npts; i++)
        rptr[i] = (double) iptr[i];
    }
    else if (sampletype == 'f')
    {
      float *fptr = (float *) seg->datasamples;
      for (int64_t i = 0; i < npts; i++)
        rptr[i] = (double) fptr[i];
    }
    else if (sampletype == 'd')
    {
      double *dptr = (double *) seg->datasamples;
      for (int64_t i = 0; i < npts; i++)
        rptr[i] = dptr[i];
    }
    else
    {
      UNPROTECT(1); /* r_data */
    Rf_error("Unsupported sample type '%c'", sampletype);
    }
    
    /* ---- Build segment list element ---- */
    SEXP r_seg = PROTECT(allocVector(VECSXP, 5));
    SEXP r_seg_names = PROTECT(allocVector(STRSXP, 5));
    
    SET_VECTOR_ELT(r_seg, 0, ScalarReal(tstart));
    SET_VECTOR_ELT(r_seg, 1, ScalarReal(tend));
    SET_VECTOR_ELT(r_seg, 2, ScalarReal(samprate));
    SET_VECTOR_ELT(r_seg, 3, ScalarReal((double)npts));
    SET_VECTOR_ELT(r_seg, 4, r_data);
    
    SET_STRING_ELT(r_seg_names, 0, mkChar("start"));
    SET_STRING_ELT(r_seg_names, 1, mkChar("end"));
    SET_STRING_ELT(r_seg_names, 2, mkChar("samprate"));
    SET_STRING_ELT(r_seg_names, 3, mkChar("npts"));
    SET_STRING_ELT(r_seg_names, 4, mkChar("data"));
    
    setAttrib(r_seg, R_NamesSymbol, r_seg_names);
    
    SET_VECTOR_ELT(r_seglist, seg_index++, r_seg);
    
    UNPROTECT(3); /* r_seg, r_seg_names, r_data */
  }
  
  /* ---- Build trace object: list(sid = ..., segments = ...) ---- */
  SEXP r_trace = PROTECT(allocVector(VECSXP, 2));
  SEXP r_trace_names = PROTECT(allocVector(STRSXP, 2));
  
  SET_VECTOR_ELT(r_trace, 0, mkString(sid));
  SET_VECTOR_ELT(r_trace, 1, r_seglist);
  
  SET_STRING_ELT(r_trace_names, 0, mkChar("sid"));
  SET_STRING_ELT(r_trace_names, 1, mkChar("segments"));
  
  setAttrib(r_trace, R_NamesSymbol, r_trace_names);
  
  SET_VECTOR_ELT(r_traces, trace_index, r_trace);
  
  UNPROTECT(3); /* r_trace, r_trace_names, r_seglist */
  
  trace_index++;
  }
  
  mstl3_free(&mstl, 1);
  
  UNPROTECT(1); /* r_traces */
  return r_traces;
}
