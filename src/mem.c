#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
#include <rxode2parseSbuf.h>
#include <errno.h>
#include <dparser2.h>
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

void monolix2rx_indDef_parseFree(int last);

extern sbuf firstErr;
extern sbuf sbErr1;
extern sbuf sbErr2;
extern sbuf sbTransErr;
extern sbuf curLine;
extern vLines _dupStrs;

void monolix2rx_full_parseFree(int last) {
  lineFree(&_dupStrs);
  if (last) {
    sFree(&firstErr);
    sFree(&sbTransErr);
    sFree(&sbErr1);
    sFree(&sbErr2);
    sFree(&curLine);
  } else {
    sClear(&firstErr);
    sClear(&sbTransErr);
    sClear(&sbErr1);
    sClear(&sbErr2);
    sClear(&curLine);
    lineIni(&_dupStrs);
  }
  monolix2rx_indDef_parseFree(last);
}


int monolix2rx_full_ini_done = 0;
void monolix2rx_full_ini(void) {
  if (monolix2rx_full_ini_done == 0) {
    sIni(&firstErr);
    sIni(&sbTransErr);
    sIni(&sbErr1);
    sIni(&sbErr2);
    sIni(&curLine);
    lineIni(&_dupStrs);
    monolix2rx_full_ini_done = 1;
  }
}

SEXP _monolix2rx_r_parseFree(void) {
  monolix2rx_full_parseFree(0);
  return R_NilValue;
}

SEXP _monolix2rx_r_parseIni(void) {
  monolix2rx_full_ini();
  return R_NilValue;
}
