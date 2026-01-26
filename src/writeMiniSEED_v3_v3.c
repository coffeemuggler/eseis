#include <R.h>
#include <Rinternals.h>
#include "libmseed/libmseed.h"
#include <string.h>
#include <stdlib.h>

SEXP writeMiniSEED_v3_v3(SEXP traces_, SEXP samprate_, SEXP starttime_,
                         SEXP sid_, SEXP reclen_, SEXP file_, SEXP encoding_)
{
  int verbose = 0;
  int total_records = 0;
  
  /* --- Extract arguments --- */
  int ntraces = LENGTH(traces_);
  const char *outfile = CHAR(STRING_ELT(file_, 0));
  const char *encstr  = CHAR(STRING_ELT(encoding_, 0));
  int reclen = INTEGER(reclen_)[0];
  
  /* --- Map encoding keyword to libmseed constant --- */
  int encoding = 0;
  if      (!strcmp(encstr, "STEIM2"))  encoding = DE_STEIM2;
  else if (!strcmp(encstr, "STEIM1"))  encoding = DE_STEIM1;
  else if (!strcmp(encstr, "INT32"))   encoding = DE_INT32;
  else if (!strcmp(encstr, "INT16"))   encoding = DE_INT16;
  else if (!strcmp(encstr, "FLOAT32")) encoding = DE_FLOAT32;
  else error("Unsupported encoding: %s", encstr);
  
  /* --- Loop over traces --- */
  for (int i = 0; i < ntraces; i++)
  {
    SEXP trace = VECTOR_ELT(traces_, i);
    int nsamp = LENGTH(trace);
    
    double samprate = REAL(samprate_)[i];
    const char *start = CHAR(STRING_ELT(starttime_, i));
    const char *sid   = CHAR(STRING_ELT(sid_, i));
    
    /* --- Allocate MS3Record --- */
    MS3Record *msr = msr3_init(NULL);
    if (!msr)
      error("Failed to allocate MS3Record");
    
    /* --- Populate header --- */
    strncpy(msr->sid, sid, sizeof(msr->sid) - 1); 
    msr->sid[sizeof(msr->sid) - 1] = '\0';
    msr->reclen = reclen;
    msr->pubversion = 1;
    msr->samprate = samprate;
    msr->starttime = ms_timestr2nstime(start);
    msr->encoding = encoding;
    
    /* --- Attach data --- */
    float *floatbuf = NULL;
    
    if (encoding == DE_FLOAT32)
    {
      /* Convert R double vector to float buffer */
      floatbuf = malloc(nsamp * sizeof(float));
      if (!floatbuf)
        error("Failed to allocate float buffer");
      
      double *src = REAL(trace);
      for (int j = 0; j < nsamp; j++)
        floatbuf[j] = (float) src[j];
      
      msr->datasamples = floatbuf;
      msr->sampletype = 'f';
    }
    else
    {
      /* Integer encodings */
      int32_t *intbuf = INTEGER(trace);
      msr->datasamples = intbuf;
      msr->sampletype = 'i';
    }
    
    msr->numsamples = nsamp;
    
    /* --- Write MiniSEED record(s) --- */
    int recs = msr3_writemseed(msr, outfile, 0, MSF_FLUSHDATA, verbose);
    total_records += recs;
    
    /* --- Prevent double-free --- */
    msr->datasamples = NULL;
    
    /* --- Cleanup --- */
    if (floatbuf)
      free(floatbuf);
    
    msr3_free(&msr);
  }
  
  return ScalarInteger(total_records);
}
