#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <rxode2parseSbuf.h>
#include <limits.h>

// Test-only entry point that drives each overflow guard in sbuf.c.
//
// Reaching the guards with real input needs a ~2GB string, so instead the
// buffers are faked: a small static backing store with the offset/size/line
// counters set close to INT_MAX.  Every guard runs before the buffer is
// resized or indexed by those counters, so nothing large is allocated, and
// since nothing is heap allocated the Rf_error() longjmp leaks nothing.
SEXP _monolix2rx_sbufGuardTest(SEXP whichSEXP) {
  static char buf[64];
  int which = Rf_asInteger(whichSEXP);
  sbuf sb;
  vLines vl;
  buf[0] = '\0';
  sb.s = buf;
  sb.sN = (int)sizeof(buf);
  sb.o = 0;
  vl.s = buf;
  vl.sN = (int)sizeof(buf);
  vl.o = 0;
  vl.n = 0;
  vl.nL = SBUF_MXLINE;
  vl.line = NULL;
  vl.lProp = NULL;
  vl.lType = NULL;
  vl.os = NULL;
  if (which == 1) {
    // sAppendN: negative length
    sAppendN(&sb, "x", -1);
  } else if (which == 2) {
    // sAppendN: string buffer overflow
    sb.o = INT_MAX - SBUF_MXBUF;
    sAppendN(&sb, "x", 1);
  } else if (which == 3) {
    // sAppend: string buffer overflow
    sb.o = INT_MAX - SBUF_MXBUF;
    sAppend(&sb, "%s", "x");
  } else if (which == 4) {
    // addLine: string buffer overflow
    vl.sN = INT_MAX - SBUF_MXBUF;
    addLine(&vl, "%s", "x");
  } else if (which == 5) {
    // addLine: line array overflow
    vl.nL = INT_MAX - SBUF_MXLINE - 1;
    vl.n = vl.nL - 2;
    addLine(&vl, "%s", "x");
  } else if (which == 6) {
    // Normal growth past the initial SBUF_MXBUF bytes with a real heap
    // buffer, returning the final length (2 * SBUF_MXBUF + 2).
    sbuf grow;
    sNull(&grow);
    sFreeIni(&grow);
    for (int i = 0; i < SBUF_MXBUF; i++) sAppendN(&grow, "a", 1);
    sAppendN(&grow, "b", 1);
    for (int i = 0; i < SBUF_MXBUF; i++) sAppend(&grow, "%s", "c");
    sAppend(&grow, "%s", "d");
    int len = grow.o;
    sFree(&grow);
    return Rf_ScalarInteger(len);
  } else {
    Rf_error("unknown sbuf guard test: %d", which);
  }
  return R_NilValue; // # nocov: every branch above either returns or longjmps
}
