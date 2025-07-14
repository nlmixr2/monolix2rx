#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
#include <rxode2parseSbuf.h>
#include <errno.h>
#include <dparserPtr.h>
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <Rmath.h>
#define _(String) (String)
#include "mlxtranInd.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_mlxtranInd;
#define curP monolix2rx_individual_curP

#define gBuf monolix2rx_individual_gBuf
#define gBufFree monolix2rx_individual_gBufFree
#define gBufLast monolix2rx_individual_gBufLast
#define _pn monolix2rx_individual__pn
#define freeP monolix2rx_individual_freeP
#define parseFreeLast monolix2rx_individual_parseFreeLast
#define parseFree monolix2rx_individual_parseFree
#define errP monolix2rx_individual_errP
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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_individual);
void wprint_node_individual(int depth, char *token_name, char *token_value, void *client_individual) {}

extern sbuf curLine;

int individual_process_catId(const char *name, D_ParseNode *pn) {
  if (!strcmp("catId", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxInputCatItem(v);
    return 1;
  }
  return 0;
}

int individual_process_inpId(const char *name, D_ParseNode *pn) {
  if (!strcmp("inpId", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxInputAdd(v);
    return 1;
  }
  return 0;
}

int individual_process_catCov(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("catCov", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxInputCat(v);
    return 1;
  }
  return 0;
}
int individual_process_regressor(const char *name, D_ParseNode *pn) {
  if (!strcmp("regressorLine", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxIndReg(v);
    return 1;
  }
  return 0;
}

int individual_process_ignore(const char *name, D_ParseNode *pn) {
  if (!strcmp("ignoreLine", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".indIgnore");
    return 1;
  }
  return 0;
}


void wprint_parsetree_individual(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  int nch = d_get_number_of_children(pn);
  if (!strcmp("filename_t3", name) ||
      !strcmp("filename_t4", name)) {
    char *v = (char*)rc_dup_str(pn->start_loc.s, pn->end);
    monolix2rxFileinfoFile(v);
    return;
  } else if (!strcmp("filename_t1", name) ||
             !strcmp("filename_t2", name)) {
    char *v = (char*)rc_dup_str(pn->start_loc.s, pn->end);
    v++;
    int len = strlen(v);
    v[len-1] = 0;
    monolix2rxFileinfoFile(v);
    return;
  } else if (individual_process_catId(name, pn) ||
             individual_process_inpId(name, pn) ||
             individual_process_regressor(name, pn) ||
             individual_process_ignore(name, pn)) {
    // return early; no need to process more
    return;
  }
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      if (individual_process_catCov(name, pn, i)) {
        // don't process this argument more
        continue;
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_individual(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_individual(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_mlxtranInd, sizeof(D_ParseNode_User));
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
    wprint_parsetree_individual(parser_tables_mlxtranInd , _pn, 0, wprint_node_individual, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_individual(SEXP in, SEXP where) {
  sClear(&curLine);
  sClear(&firstErr);
  record = R_CHAR(STRING_ELT(where, 0));
  trans_individual(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
