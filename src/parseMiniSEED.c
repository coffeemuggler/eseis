/***************************************************************************
 * parseMiniSEED.c
 *
 * R extension:  C wrapper around msr_parse that returns a list trace
 * segments.
 *
 * Written by Jonathan Callahan, Mazama Science
 *
 * modified 2012-11-30
 *
 * Each trace segment contains information needed to populate an object of
 * class 'Trace' as defined in the 'IRISSeismic' package:
 *
 *   representation(sampling_rate = "numeric",
 *                  delta = "numeric",
 *                  calib = "numeric",
 *                  npts = "integer",
 *                  network = "character",
 *                  location = "character",
 *                  station = "character",
 *                  channel = "character",
 *                  quality = "character",
 *                  starttime = "POSIXct",
 *                  endtime = "POSIXct"),
 *
 ***************************************************************************/

#include <R.h>
#include <Rdefines.h>

#include <stdio.h>
#include <libmseed.h>

typedef void (* R_callback)(char *);

SEXP parseMiniSEED (SEXP buffer) {

  int debug = 0;
  int loopPROTECT = 0;
  int bufferLength = 0;
  char *bufferPtr;
  char msgPrefix[11] = "libmseed__";

  // Redirect libmseed logging messages to Matlab functions
  ms_loginit( (R_callback)&Rprintf, msgPrefix, (R_callback)&Rf_error, msgPrefix);

  /* Read leap second list file if env. var. LIBMSEED_LEAPSECOND_FILE is set */
  if (leapsecondlist == NULL) {
    ms_readleapseconds ("LIBMSEED_LEAPSECOND_FILE");
  }

  // Allocate space for the buffer
  PROTECT(buffer = AS_RAW(buffer));
  bufferPtr = (char *)RAW_POINTER(buffer);
  bufferLength = LENGTH(buffer);

  // Define variables to match definitions in libmseed.h
  long long int bsize = (long long int) bufferLength;
  long long int boffset = 0;

  MSRecord *msr = NULL;
  MSTraceList *mstl = NULL;
  MSTraceID *id = NULL;
  MSTraceSeg *seg = NULL;

  // Initialize the MSTraceList
  if ( ! (mstl = mstl_init (mstl)) ) {
    Rf_error("%sCannot allocate trace list.", msgPrefix);
  }

  // Set up behavior flags for msr_parse
  static int reclen  = -1;  // use automatic detection of record length
  static flag dataflag = 1; // unpack data samples
  static flag verbose = 0;  // don't spit out messages

  // Set up variables to store information accumulated per MSRecord
  int64_t totalRecords __attribute__((unused)) = 0;

  //int64_t totalSamples = 0;

  // Set up quality variables
  uint8_t b = 0;
  int total_act_flags[8];
  int total_io_flags[8];
  int total_dq_flags[8];
  for (int i=0; i<8; i++) {
    total_act_flags[i] = 0;
    total_io_flags[i] = 0;
    total_dq_flags[i] = 0;
  }
  int totalBlkt1001 = 0;
  int totalTimingQuality = 0;

  // Parse the data in the buffer
  // NOTE:  Each MSRecord is 256 bytes by default.  Many MSRecords per miniSEED file.
  boffset = 0;
  while ( boffset < bsize ) {
    if ( msr_parse (bufferPtr+boffset, bsize-boffset, &msr, reclen, dataflag, verbose) ) {
      boffset += 256;
    } else {
      totalRecords++;

      b = msr->fsdh->act_flags;
      total_act_flags[0] += (int) bit(b,0x01);
      total_act_flags[1] += (int) bit(b,0x02);
      total_act_flags[2] += (int) bit(b,0x04);
      total_act_flags[3] += (int) bit(b,0x08);
      total_act_flags[4] += (int) bit(b,0x10);
      total_act_flags[5] += (int) bit(b,0x20);
      total_act_flags[6] += (int) bit(b,0x40);
      total_act_flags[7] += (int) bit(b,0x80);

      b = msr->fsdh->io_flags;
      total_io_flags[0] += (int) bit(b,0x01);
      total_io_flags[1] += (int) bit(b,0x02);
      total_io_flags[2] += (int) bit(b,0x04);
      total_io_flags[3] += (int) bit(b,0x08);
      total_io_flags[4] += (int) bit(b,0x10);
      total_io_flags[5] += (int) bit(b,0x20);
      total_io_flags[6] += (int) bit(b,0x40);
      total_io_flags[7] += (int) bit(b,0x80);

      b = msr->fsdh->dq_flags;
      total_dq_flags[0] += (int) bit(b,0x01);
      total_dq_flags[1] += (int) bit(b,0x02);
      total_dq_flags[2] += (int) bit(b,0x04);
      total_dq_flags[3] += (int) bit(b,0x08);
      total_dq_flags[4] += (int) bit(b,0x10);
      total_dq_flags[5] += (int) bit(b,0x20);
      total_dq_flags[6] += (int) bit(b,0x40);
      total_dq_flags[7] += (int) bit(b,0x80);

      if ( msr->Blkt1001 ) {
        totalBlkt1001++;
        totalTimingQuality += (int) msr->Blkt1001->timing_qual;
      }

      mstl_addmsr ( mstl, msr, 0, 1, -1.0, -1.0 );

      boffset += msr->reclen;
    }
  }

  // Sanity check
  if (mstl->numtraces == 0) {
    Rf_error("%szero traces in miniSEED record.", msgPrefix);
  }
  if (mstl->numtraces > 1) {
    Rf_error("%s%d traces in miniSEED record.  Only 1 is currently supported.", msgPrefix, mstl->numtraces);
  }

  if (debug) {
    Rprintf("%sbufferLength = %d, %d traces found\n", msgPrefix, bufferLength, mstl->numtraces);
    mstl_printtracelist ( mstl, 1, 1, 1 );
  }

  // Get pointers to the first trace and first segment
  id = mstl->traces;
  seg = id->first;

  // Create the return list, each element of which will contain a segmentList
  SEXP returnList;
  PROTECT(returnList = NEW_LIST(id->numsegments));
  //numPROTECT++;

  if (debug) {
    Rprintf("numsegments %d\n", id->numsegments);
  }

  // Set up the names that go in the returnList
  char *names[14] = {"npts","sampling_rate","network","station","location","channel","quality",
                     "starttime","endtime","data","act_flags","io_flags","dq_flags","timing_qual"};
  SEXP listNames;
  PROTECT(listNames = NEW_CHARACTER(14));
  //numPROTECT++;
  for (int i=0; i<14; i++) {
    SET_STRING_ELT(listNames, i, mkChar(names[i]));
  }

  // Loop through the segments (continuous blocks of data), populating a segmentList
  // which is then inserted into the returnList.
  for (int segIndex=0; segIndex < id->numsegments; segIndex++ ) {

    if (debug) {
      Rprintf("segIndex %d\n", segIndex);
    }

    // Now set up the variable vectors that go into the segmentList
    SEXP npts, sampling_rate, network, station, location, channel, quality, starttime, endtime, data;

    loopPROTECT = 0;

    PROTECT(npts = NEW_INTEGER(1));
    loopPROTECT++;
    INTEGER(npts)[0] = (int) seg->samplecnt;

    PROTECT(sampling_rate = NEW_NUMERIC(1));
    loopPROTECT++;
    // REC -- modified samprate for segment
    // -- get the mode value of a set of sample rates recorded for this segment
    REAL(sampling_rate)[0] = get_segsamprate_mode(seg->samprate_list);

    PROTECT(network = NEW_CHARACTER(1));
    loopPROTECT++;
    SET_STRING_ELT(network, 0, mkChar(id->network));

    PROTECT(station = NEW_CHARACTER(1));
    loopPROTECT++;
    SET_STRING_ELT(station, 0, mkChar(id->station));

    PROTECT(location = NEW_CHARACTER(1));
    loopPROTECT++;
    SET_STRING_ELT(location, 0, mkChar(id->location));

    PROTECT(channel = NEW_CHARACTER(1));
    loopPROTECT++;
    SET_STRING_ELT(channel, 0, mkChar(id->channel));

    PROTECT(quality = NEW_CHARACTER(1));
    loopPROTECT++;
    // Extra step because R only deals with strings, not single characters.
    // Create the NULL terminated string by hand.
    char dataquality[2] = { id->dataquality, '\0' };
    // // //Rprintf("dataquality is '%c'\n",id->dataquality);
    SET_STRING_ELT(quality, 0, mkChar(dataquality));

    // NOTE:  When these values are used in R, starttime and endtime need to be converted with:
    //  s <- as.POSIXct(header$starttime, tz="GMT", origin=as.POSIXct("1970-01-01T00:00:00Z"))
    PROTECT(starttime = NEW_NUMERIC(1));
    loopPROTECT++;
    REAL(starttime)[0] = (double) MS_HPTIME2EPOCH(seg->starttime);

    PROTECT(endtime = NEW_NUMERIC(1));
    loopPROTECT++;
    REAL(endtime)[0] = (double) MS_HPTIME2EPOCH(seg->endtime);

    // TODO:  Check for non-int data types
    // // //Rprintf("Data is of type '%c'\n",seg->sampletype);
    PROTECT(data = NEW_NUMERIC(seg->samplecnt));
    loopPROTECT++;

    int32_t *idatasamplesPtr = (int32_t *) seg->datasamples;
    float *fdatasamplesPtr = (float *) seg->datasamples;
    double *ddatasamplesPtr = (double *) seg->datasamples;

    double *dataPtr = NUMERIC_POINTER(data);
    for (int i=0; i<seg->samplecnt; i++) {
      if ( seg->sampletype == 'i' ) {
        dataPtr[i] = (double) idatasamplesPtr[i];
      } else if ( seg->sampletype == 'f' ) {
        dataPtr[i] = (double) fdatasamplesPtr[i];
      } else if ( seg->sampletype == 'd' ) {
        dataPtr[i] = (double) ddatasamplesPtr[i];
      } else {
        Rf_error("%s Data in miniSEED record is of type '%c'.  Must be 'i', 'f' or 'd'.", msgPrefix, seg->sampletype);
      }
    }

    // Now deal with the quality flags
    //uint8_t b = 0;
    SEXP act_flags, io_flags, dq_flags, timing_qual;

    PROTECT(act_flags = NEW_INTEGER(8));
    loopPROTECT++;
    for (int i=0; i<8; i++) {
      INTEGER(act_flags)[i] = (int) total_act_flags[i];
    }

    PROTECT(io_flags = NEW_INTEGER(8));
    loopPROTECT++;
    for (int i=0; i<8; i++) {
      INTEGER(io_flags)[i] = (int) total_io_flags[i];
    }

    PROTECT(dq_flags = NEW_INTEGER(8));
    loopPROTECT++;
    for (int i=0; i<8; i++) {
      INTEGER(dq_flags)[i] = (int) total_dq_flags[i];
    }

    PROTECT(timing_qual = NEW_NUMERIC(1));
    loopPROTECT++;
    if (totalBlkt1001 > 0) {
      REAL(timing_qual)[0] = (double) totalTimingQuality / (double) totalBlkt1001;
    } else {
      REAL(timing_qual)[0] = NA_REAL;
    }

    // All the data have been parsed.  Now set up and fill the segmentList
    SEXP segmentList;
    PROTECT(segmentList = NEW_LIST(14));
    loopPROTECT++;
    SET_VECTOR_ELT(segmentList, 0, npts);
    SET_VECTOR_ELT(segmentList, 1, sampling_rate);
    SET_VECTOR_ELT(segmentList, 2, network);
    SET_VECTOR_ELT(segmentList, 3, station);
    SET_VECTOR_ELT(segmentList, 4, location);
    SET_VECTOR_ELT(segmentList, 5, channel);
    SET_VECTOR_ELT(segmentList, 6, quality);
    SET_VECTOR_ELT(segmentList, 7, starttime);
    SET_VECTOR_ELT(segmentList, 8, endtime);
    SET_VECTOR_ELT(segmentList, 9, data);
    SET_VECTOR_ELT(segmentList, 10, act_flags);
    SET_VECTOR_ELT(segmentList, 11, io_flags);
    SET_VECTOR_ELT(segmentList, 12, dq_flags);
    SET_VECTOR_ELT(segmentList, 13, timing_qual);
    setAttrib(segmentList, R_NamesSymbol, listNames);

    // Put the segmentList inside the returnList;
    SET_VECTOR_ELT(returnList, segIndex, segmentList);

    // REC -- now UNPROTECT all elements built up in the loop
    // data should now protected by returnList
    UNPROTECT(loopPROTECT);  // pop the stack down to pre-loop count

    // Next segment
    seg = seg->next;

    if (seg==NULL){
       break;
    }
  }

  /* Make sure the miniSEED stuff is cleaned up */
  ms_readmsr (&msr, NULL, 0, NULL, NULL, 0, 0, 0);
  mstl_free (&mstl, 1);

  /* Unprotect and return */
  //UNPROTECT(numPROTECT);
  UNPROTECT(3);

  return(returnList);
}


