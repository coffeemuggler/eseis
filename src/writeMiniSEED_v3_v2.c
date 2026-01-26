#include <R.h>
#include <Rinternals.h>
#include "libmseed/libmseed.h"
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

SEXP writeMiniSEED_v3_v2(SEXP traces_, SEXP samprate_, SEXP starttime_,
                         SEXP sid_, SEXP reclen_, SEXP file_, SEXP encoding_)
{
  int verbose = 0;
  int64_t total_records = 0;
  
  /* --- Extract arguments from R --- */
  int ntraces         = LENGTH(traces_);
  const char *outfile = CHAR(STRING_ELT(file_, 0));
  const char *encstr  = CHAR(STRING_ELT(encoding_, 0));
  int rec_len         = INTEGER(reclen_)[0];
  
  /* --- Map encoding keyword to libmseed constant --- */
  int16_t encoding = 0;
  if      (!strcmp(encstr, "STEIM2"))  encoding = DE_STEIM2;
  else if (!strcmp(encstr, "STEIM1"))  encoding = DE_STEIM1;
  else if (!strcmp(encstr, "INT32"))   encoding = DE_INT32;
  else if (!strcmp(encstr, "INT16"))   encoding = DE_INT16;
  else if (!strcmp(encstr, "FLOAT32")) encoding = DE_FLOAT32;
  else error("Unsupported encoding: %s", encstr);
  
  for (int i = 0; i < ntraces; i++)
  {
    SEXP trace = VECTOR_ELT(traces_, i);
    int64_t nsamp = (int64_t) LENGTH(trace);
    
    double samprate      = REAL(samprate_)[i];
    const char *startstr = CHAR(STRING_ELT(starttime_, i));
    const char *sid      = CHAR(STRING_ELT(sid_, i));
    
    MS3Record *msr = msr3_init(NULL);
    if (!msr)
      error("Failed to allocate MS3Record");
    
    /* --- Header --- */
    ms_strncpclean(msr->sid, sid, sizeof(msr->sid));
    msr->reclen     = rec_len;
    msr->pubversion = 1;
    msr->samprate   = samprate;
    msr->starttime  = ms_timestr2nstime(startstr);
    msr->encoding   = encoding;
    msr->numsamples = nsamp;
    msr->samplecnt  = nsamp;
    /* NOTE: do NOT set msr->formatversion here; let libmseed decide from flags */
    
    void *databuf = NULL;
    char sampletype = 0;
    
    if (encoding == DE_FLOAT32)
    {
      /* R numeric -> float32 */
      double *src = REAL(trace);
      float *fbuf = (float *) malloc(nsamp * sizeof(float));
      if (!fbuf)
        error("Failed to allocate float buffer");
      
      for (int64_t j = 0; j < nsamp; j++)
        fbuf[j] = (float) src[j];
      
      databuf    = fbuf;
      sampletype = 'f';
    }
    else if (encoding == DE_INT32 || encoding == DE_STEIM1 || encoding == DE_STEIM2)
    {
      /* R integer (32‑bit) -> own int32 buffer */
      int *src = INTEGER(trace);
      int32_t *ibuf = (int32_t *) malloc(nsamp * sizeof(int32_t));
      if (!ibuf)
        error("Failed to allocate int32 buffer");
      
      for (int64_t j = 0; j < nsamp; j++)
        ibuf[j] = (int32_t) src[j];
      
      databuf    = ibuf;
      sampletype = 'i';
    }
    else if (encoding == DE_INT16)
    {
      /* Need explicit 16‑bit buffer */
      int *src = INTEGER(trace);
      int16_t *sbuf = (int16_t *) malloc(nsamp * sizeof(int16_t));
      if (!sbuf)
        error("Failed to allocate int16 buffer");
      
      for (int64_t j = 0; j < nsamp; j++)
        sbuf[j] = (int16_t) src[j];
      
      databuf    = sbuf;
      sampletype = 'i';  /* integer samples; encoding tells libmseed it's 16‑bit */
    }
    
    msr->datasamples = databuf;
    msr->sampletype  = sampletype;
    
    /* --- Write as MiniSEED 2 --- */
    uint32_t flags = MSF_FLUSHDATA | MSF_PACKVER2;
    int append = (i > 0) ? 1 : 0;
    
    int64_t recs = msr3_writemseed(msr, outfile, append, flags, verbose);
    if (recs < 0)
    {
      const char *estr = ms_errorstr((int)recs);
      msr->datasamples = NULL;
      msr3_free(&msr);
      if (encoding == DE_FLOAT32 || encoding == DE_INT16 ||
          encoding == DE_INT32 || encoding == DE_STEIM1 || encoding == DE_STEIM2)
        free(databuf);
      error("msr3_writemseed() failed: %s", estr);
    }
    
    total_records += recs;
    
    /* Avoid double-free of user memory */
    msr->datasamples = NULL;
    
    if (encoding == DE_FLOAT32 || encoding == DE_INT16 ||
        encoding == DE_INT32 || encoding == DE_STEIM1 || encoding == DE_STEIM2)
      free(databuf);
    
    msr3_free(&msr);
  }
  
  return ScalarInteger((int) total_records);
}
