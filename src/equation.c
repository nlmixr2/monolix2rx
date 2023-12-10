#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
#include <rxode2parseSbuf.h>
#include <errno.h>
#include <dparser.h>
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
#include "equation.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables monolixLongEq;


D_Parser *curP=NULL;
D_Parser *errP=NULL;
D_ParseNode *_pn = 0;

char* gBuf;
int gBufLast = 0;
int gBufFree = 0;

sbuf curLine;
int abbrecAddSeq = 0;
int abbrecAddNameItem = 0;

sbuf firstErr;


void freeP(void){
  if (_pn){
    free_D_ParseTreeBelow(curP,_pn);
    free_D_ParseNode(curP,_pn);
  }
  _pn=0;
  if (curP != NULL){
    free_D_Parser(curP);
  }
  curP = NULL;
}
void parseFreeLast(void) {
  if (gBufFree) R_Free(gBuf);
  //sFree(&sbOut);
  freeP();
  //sFree(&_bufw);
  //sFree(&_bufw2);
}
void parseFree(int last) {
  freeP();
  if (last){
    parseFreeLast();
  }
}

void trans_longEq(const char* parse){
  freeP();
  curP = new_D_Parser(&monolixLongEq, sizeof(D_ParseNode_User));
  curP->save_parse_tree = 1;
  curP->error_recovery = 1;
  curP->initial_scope = NULL;
  //curP->syntax_error_fn = nonmem2rxSyntaxError;
  if (gBufFree) R_Free(gBuf);
  // Should be able to use gBuf directly, but I believe it cause
  // problems with R's garbage collection, so duplicate the string.
  gBuf = (char*)(parse);
  //eBuf = gBuf;
  errP = curP;
  /* eBufLast = 0; */
  gBufFree=0;
  _pn= dparse(curP, gBuf, (int)strlen(gBuf));
  if (!_pn || curP->syntax_errors) {
  } else {
    /* wprint_parsetree_abbrec(parser_tables_nonmem2rxAbbrevRec , _pn, 0, wprint_node_abbrec, NULL); */
  }
  /* finalizeSyntaxError(); */
}

SEXP _monolix2rx_trans_longEq(SEXP in) {
  sClear(&curLine);
  sClear(&firstErr);
  trans_longEq(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
