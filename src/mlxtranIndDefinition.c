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
#include "mlxtranIndDefinition.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_mlxtranIndDefinition;
#define curP monolix2rx_indDef_curP

#define gBuf monolix2rx_indDef_gBuf
#define gBufFree monolix2rx_indDef_gBufFree
#define gBufLast monolix2rx_indDef_gBufLast
#define _pn monolix2rx_indDef__pn
#define freeP monolix2rx_indDef_freeP
#define parseFreeLast monolix2rx_indDef_parseFreeLast
#define parseFree monolix2rx_indDef_parseFree
#define errP monolix2rx_indDef_errP
#include "parseSyntaxErrors.h"
#include "util.h"

char* gBuf;
int gBufLast = 0;
int gBufFree = 0;
D_Parser *curP=NULL;
D_Parser *errP=NULL;
D_ParseNode *_pn = 0;

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

extern char * rc_dup_str(const char *s, const char *e);
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_indDef);
void wprint_node_indDef(int depth, char *token_name, char *token_value, void *client_indDef) {}

extern sbuf curLine;

int indDef_process_varName(const char* name, D_ParseNode *pn) {
  if (!strcmp("otherLine", name) ||
      !strcmp("logitNormalLine", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxAddVar(v);
    return 1;
  }
  return 0;
}

int indDef_process_covItem(const char *name, D_ParseNode *pn) {
  if (!strcmp("covId", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxAddCov(v);
    return 1;
  }
  return 0;
}

int indDef_process_distribution(const char* name, D_ParseNode *pn) {
  if (!strcmp("logitNormalDist", name) ||
      !strcmp("distribution", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetDist(v);
    return 1;
  }
  return 0;
}

int indDef_process_sdDef(const char *name, D_ParseNode *pn) {
  if (!strcmp("sdItem", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetSd(v);
    return 1;
  }
  return 0;
}

int indDef_process_varDef(const char *name, D_ParseNode *pn) {
  if (!strcmp("varItem", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetVar(v);
    return 1;
  }
  return 0;
}

int indDef_process_minDef(const char *name, D_ParseNode *pn) {
  if (!strcmp("minVal", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetMin(v);
    return 1;
  }
  return 0;
}

int indDef_process_maxDef(const char *name, D_ParseNode *pn) {
  if (!strcmp("maxVal", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetMax(v);
    return 1;
  }
  return 0;
}

int intDef_handle_typical_def(const char* name,  D_ParseNode *pn) {
  int tv=0, mv=0, tf=0, mf=0;
  if ((tv = !strcmp("typicalVar", name)) ||
      (tf = !strcmp("typicalFixed", name)) ||
      (mv = !strcmp("meanVar", name)) ||
      (mf = !strcmp("meanFixed", name))) {
    int mean = mv || mf;
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (tv || mv) {
      // estimated value
      monolix2rxSetDistTypicalEst(v, mean);
      return 1;
    }
    // fixed value
    monolix2rxSetDistTypicalFixed(v, mean);
    return 1;
  }
  return 0;
}

int indDef_process_iov(const char* name, D_ParseNode *pn) {
  if (!strcmp("iovItemVL", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetIov(v);
    return 1;
  }
  return 0;
}

int indDef_process_coefListStart(const char* name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("coeffList", name)) {
    monolix2rxPushCoefList();
    return 1;
  }
  return 0;
}

int indDef_process_coefSingle(const char* name, D_ParseNode *pn) {
  if (!strcmp("coefItem", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxAddCoefSingle(v);
    return 1;
  }
  return 0;
}

int indDef_process_coefItemList(const char* name, D_ParseNode *pn) {
  if (!strcmp("coefItemL", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxAddCoefMult(v);
    return 1;
  }
  return 0;
}

int indDef_process_corr(const char* name, D_ParseNode *pn) {
  if (!strcmp("corrOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v1 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 4);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 7);
    char *v3 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxAddCor(v1, v2, v3);
    return 1;
  }
  return 0;
}

int indDef_process_corReset(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("corLine", name)) {
    const char *v = "id";
    monolix2rxSetCorLevel(v);
    return 1;
  }
  return 0;
}

int indDef_process_corrLevel(const char *name, D_ParseNode *pn) {
  if (!strcmp("corIovItem", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetCorLevel(v);
    return 1;
  }
  return 0;
}

void wprint_parsetree_indDef(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  int nch = d_get_number_of_children(pn);
  indDef_process_varName(name, pn); // need to process more

  if (indDef_process_distribution(name, pn) ||
      intDef_handle_typical_def(name, pn) ||
      indDef_process_sdDef(name, pn) ||
      indDef_process_varDef(name, pn) ||
      indDef_process_minDef(name, pn) ||
      indDef_process_maxDef(name, pn) ||
      indDef_process_iov(name, pn) ||
      indDef_process_covItem(name, pn) ||
      indDef_process_coefSingle(name, pn) ||
      indDef_process_coefItemList(name, pn) ||
      indDef_process_corr(name, pn) ||
      indDef_process_corrLevel(name, pn)) {
    // return early; no need to process more
    return;
  }
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      if (indDef_process_coefListStart(name, pn, i) ||
          indDef_process_corReset(name, pn, i)) {
        // don't process this argument more
        continue;
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_indDef(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_indDef(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_mlxtranIndDefinition, sizeof(D_ParseNode_User));
  curP->save_parse_tree = 1;
  curP->error_recovery = 1;
  curP->initial_scope = NULL;
  curP->syntax_error_fn = monolix2rxSyntaxError;
  if (gBufFree) R_Free(gBuf);
  // Should be able to use gBuf directly, but I believe it cause
  // problems with R's garbage collection, so duplicate the string.
  gBuf = (char*)(parse);
  eBuf = gBuf;
  errP = curP;
  eBufLast = 0;
  gBufFree=0;
  _pn= dparse(curP, gBuf, (int)strlen(gBuf));
  if (!_pn || curP->syntax_errors) {
  } else {
    wprint_parsetree_indDef(parser_tables_mlxtranIndDefinition , _pn, 0, wprint_node_indDef, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_indDef(SEXP in) {
  sClear(&curLine);
  sClear(&firstErr);
  record = "mlxtran [INDIVIDUAL] DEFINITION:";
  trans_indDef(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
