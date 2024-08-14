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
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("monolix2rx", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include "parseSyntaxErrors.h"

// These are the shared variables

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
vLines _dupStrs;
char * rc_dup_str(const char *s, const char *e) {
  lastStr=s;
  int l = e ? e-s : (int)strlen(s);
  //syntaxErrorExtra=min(l-1, 40);
  addLine(&_dupStrs, "%.*s", l, s);
  return _dupStrs.line[_dupStrs.n-1];
}
