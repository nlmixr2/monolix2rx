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
#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("monolix2rx", String)
/* replace pkg as appropriate */
#else
#define _(String) (String)
#endif
#include "mlxtranOp.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_mlxtranOp;
#define curP monolix2rx_mlxtran_op_curP

#define gBuf monolix2rx_mlxtran_op_gBuf
#define gBufFree monolix2rx_mlxtran_op_gBufFree
#define gBufLast monolix2rx_mlxtran_op_gBufLast
#define _pn monolix2rx_mlxtran_op__pn
#define freeP monolix2rx_mlxtran_op_freeP
#define parseFreeLast monolix2rx_mlxtran_op_parseFreeLast
#define parseFree monolix2rx_mlxtran_op_parseFree
#define errP monolix2rx_mlxtran_op_errP
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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_mlxtran_op);
void wprint_node_mlxtran_op(int depth, char *token_name, char *token_value, void *client_mlxtran_op) {}

extern sbuf curLine;

int mlxtran_op_process_logicalOp(const char *name, D_ParseNode *pn) {
  if (!strcmp("logicalOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxDouble(v, v2, ".mlxtranLogicalOp");
    return 1;
  }
  return 0;
}

int mlxtran_op_process_charOp1(const char *name, D_ParseNode *pn) {
  if (!strcmp("charOp1", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxDouble(v, v2, ".mlxtranCharOp");
    return 1;
  }
  return 0;
}

int mlxtran_op_process_charOp2(const char *name, D_ParseNode *pn) {
  if (!strcmp("charOp2", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s + 1, xpn->end - 1);
    monolix2rxDouble(v, v2, ".mlxtranCharOp");
    return 1;
  }
  return 0;
}

int mlxtran_op_process_numOp(const char *name, D_ParseNode *pn) {
  if (!strcmp("numOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 2);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxDouble(v, v2, ".mlxtranNumOp");
    return 1;
  }
  return 0;
}

int mlxtran_op_process_all_val(const char *name, D_ParseNode *pn) {
  if (!strcmp("all_val", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    int quote = 0;
    if (v[0] == '\'' || v[0] == '"') {
      quote=1;
      v++;
      char *v2 = v;
      while(v2[0] != 0) {
        v2++;
      }
      v2--;
      v2[0] =0;
    }
    monolix2rxDoubleI(v, quote, ".mlxtranListVal");
    return 1;
  }
  return 0;
}
int mlxtran_op_process_listOp(const char *name, D_ParseNode *pn, int i) {
  if (i ==0 && !strcmp("listOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".mlxtranListOp");
    return 1;
  }
  return 0;
}

void wprint_parsetree_mlxtran_op(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  int nch = d_get_number_of_children(pn);
  if (mlxtran_op_process_logicalOp(name, pn) ||
      mlxtran_op_process_charOp1(name, pn) ||
      mlxtran_op_process_charOp2(name, pn) ||
      mlxtran_op_process_numOp(name, pn) ||
      mlxtran_op_process_all_val(name, pn)) {
    // return early; no need to process more
    return;
  }
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      if (mlxtran_op_process_listOp(name, pn, i)) {
        continue;
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_mlxtran_op(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_mlxtran_op(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_mlxtranOp, sizeof(D_ParseNode_User));
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
    wprint_parsetree_mlxtran_op(parser_tables_mlxtranOp , _pn, 0, wprint_node_mlxtran_op, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_mlxtran_op(SEXP in, SEXP what) {
  sClear(&curLine);
  sClear(&firstErr);
  record = R_CHAR(STRING_ELT(what, 0));
  trans_mlxtran_op(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
