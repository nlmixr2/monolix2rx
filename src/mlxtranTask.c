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
#include "mlxtranTask.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_mlxtranTask;
#define curP monolix2rx_mlxtrantask_curP

#define gBuf monolix2rx_mlxtrantask_gBuf
#define gBufFree monolix2rx_mlxtrantask_gBufFree
#define gBufLast monolix2rx_mlxtrantask_gBufLast
#define _pn monolix2rx_mlxtrantask__pn
#define freeP monolix2rx_mlxtrantask_freeP
#define parseFreeLast monolix2rx_mlxtrantask_parseFreeLast
#define parseFree monolix2rx_mlxtrantask_parseFree
#define errP monolix2rx_mlxtrantask_errP
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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_mlxtrantask);
void wprint_node_mlxtrantask(int depth, char *token_name, char *token_value, void *client_mlxtrantask) {}

extern sbuf curLine;

int mlxtrantask_process_funName(const char *name, D_ParseNode *pn) {
  if (!strcmp("funName", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".taskFun");
    return 1;
  }
  return 0;
}

int mlxtrantask_process_funList(const char *name, D_ParseNode *pn, int i) {
  if (i ==0 && !strcmp("funMultiOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".taskArgList");
    return 1;
  }
  return 0;
}

int mlxtrantask_process_funOp(const char *name, D_ParseNode *pn, int i) {
  if (i ==0 && !strcmp("funOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".taskArgChar");
    return 1;
  }
  return 0;
}

int mlxtrantask_process_varOp(const char *name, D_ParseNode *pn) {
  if (!strcmp("varOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".taskVal");
    return 1;
  }
  return 0;
}

void wprint_parsetree_mlxtrantask(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  int nch = d_get_number_of_children(pn);
  if (mlxtrantask_process_funName(name, pn) ||
      mlxtrantask_process_varOp(name, pn)) {
    return;
  }
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      if (mlxtrantask_process_funList(name, pn, i) ||
          mlxtrantask_process_funOp(name, pn, i)) {
        continue;
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_mlxtrantask(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_mlxtrantask(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_mlxtranTask, sizeof(D_ParseNode_User));
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
    wprint_parsetree_mlxtrantask(parser_tables_mlxtranTask , _pn, 0, wprint_node_mlxtrantask, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_mlxtrantask(SEXP in) {
  sClear(&curLine);
  sClear(&firstErr);
  record = "<MONOLIX> [TASKS]";
  trans_mlxtrantask(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
