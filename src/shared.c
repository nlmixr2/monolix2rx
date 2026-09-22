#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
#include <rxode2parseSbuf.h>
#include <errno.h>

#define iniDparserPtr _monolix2rx_iniDparserPtr
#include <dparserPtr.h>
dparserPtrIni

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#define _(String) (String)
#include "parseSyntaxErrors.h"

// These are the shared variables
// NOTE: These globals are intentionally not mutex-protected.
// R's interpreter is single-threaded; its memory allocator (R_Calloc, R_Free)
// and error handling (Rf_error) are not safe to call from multiple threads.
// All parse operations must occur on the R main thread.

const char *record;
int _rxode2_reallyHasAfter = 0;
int rx_suppress_syntax_info = 0;
sbuf sbErr1;
sbuf sbErr2;
sbuf sbTransErr;
sbuf firstErr;
char *eBuf;
int eBufFree=0;
int eBufLast=0;
int syntaxErrorExtra = 0;
int isEsc=0;
int lastSyntaxErrorLine=0;

sbuf curLine;

const char *lastStr;
int lastStrLoc=0;
// Each duplicated string is its own allocation.  Callers keep the returned
// pointers across later rc_dup_str() calls (e.g. `v` then `v2`, or curDdt), so
// the strings must never move; only the array of pointers is reallocated.
static char **_dupStrs = NULL;
static int _dupStrsN = 0, _dupStrsMax = 0;

void monolix2rx_dupStrsFree(void) {
  for (int i = 0; i < _dupStrsN; i++) R_Free(_dupStrs[i]);
  if (_dupStrs != NULL) R_Free(_dupStrs);
  _dupStrs = NULL; // R_Free() already does this; explicit for clarity
  _dupStrsN = _dupStrsMax = 0;
}

char * rc_dup_str(const char *s, const char *e) {
  lastStr=s;
  int l;
  if (e) {
    ptrdiff_t diff = e - s;
    if (diff < 0 || diff > (ptrdiff_t)INT_MAX) {
      Rf_error(_("string segment too long in rc_dup_str"));
    }
    l = (int)diff;
  } else {
    size_t slen = strlen(s);
    if (slen > (size_t)INT_MAX) {
      Rf_error(_("string too long in rc_dup_str"));
    }
    l = (int)slen;
  }
  if (_dupStrsN == _dupStrsMax) {
    if (_dupStrsMax > INT_MAX / 2 - 1024) {
      Rf_error(_("too many strings in rc_dup_str"));
    }
    int mx = _dupStrsMax * 2 + 1024;
    _dupStrs = R_Realloc(_dupStrs, mx, char*);
    _dupStrsMax = mx;
  }
  char *ret = R_Calloc((size_t)l + 1, char);
  memcpy(ret, s, l);
  ret[l] = '\0';
  _dupStrs[_dupStrsN++] = ret;
  return ret;
}
