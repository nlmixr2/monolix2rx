#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
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
#include "mlxtranPk.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_mlxtranPk;
#define curP monolix2rx_mlxtran_pk_curP

#define gBuf monolix2rx_mlxtran_pk_gBuf
#define gBufFree monolix2rx_mlxtran_pk_gBufFree
#define gBufLast monolix2rx_mlxtran_pk_gBufLast
#define _pn monolix2rx_mlxtran_pk__pn
#define freeP monolix2rx_mlxtran_pk_freeP
#define parseFreeLast monolix2rx_mlxtran_pk_parseFreeLast
#define parseFree monolix2rx_mlxtran_pk_parseFree
#define errP monolix2rx_mlxtran_pk_errP
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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_mlxtran_pk);
void wprint_node_mlxtran_pk(int depth, char *token_name, char *token_value, void *client_mlxtran_pk) {}

extern sbuf curLine;

int mlxtran_pk_process_pkmodel1(const char* name, D_ParseNode *pn, int i) {
  if (!strcmp("pkmodel1", name)) {
    if (i == 0) {
      D_ParseNode *xpn = d_get_child(pn, 0);
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      monolix2rxSingle(v, ".pkSetCc");
      return 1;
    } else if (i == 1 && i == 2) {
      return 1;
    }
  }
  return 0;
}

int mlxtran_pk_process_pkmodel2(const char* name, D_ParseNode *pn, int i) {
  if (!strcmp("pkmodel2", name)) {
    if (i == 0) {
      D_ParseNode *xpn = d_get_child(pn, 1);
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      monolix2rxSingle(v, ".pkSetCc");
      xpn = d_get_child(pn, 3);
      v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      monolix2rxSingle(v, ".pkSetCe");
      monolix2rxSingle("pkmodel", ".pkSetStatement");
      return 1;
    } else if (i == 1 && i == 2 && i == 3 && i == 4 && i == 5 && i == 6) {
      return 1;
    }
  }
  return 0;
}

int mlxtran_pk_process_declarePars(const char* name, D_ParseNode *pn) {
  if (!strcmp("pkpars0", name) ||
      !strcmp("pkparsE0", name) ||
      !strcmp("ke0Op", name) ||
      !strcmp("ktrOp", name) ||
      !strcmp("TlagOp", name) ||
      !strcmp("pOp", name) ||
      !strcmp("Tk0Op", name) ||
      !strcmp("kaOp", name) ||
      !strcmp("KtrOp", name) ||
      !strcmp("MttOp", name) ||
      !strcmp("kOp", name) ||
      !strcmp("clOp", name) ||
      !strcmp("vmOp", name) ||
      !strcmp("kmOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".pkParDeclare");
    monolix2rxSingle("pkmodel", ".pkSetStatement");
    return 1;
  }
  return 0;
}

int mlxtran_pk_process_eqExpr(const char* name, D_ParseNode *pn) {
  if (!strcmp("eqExpr", name)) {
    D_ParseNode *xpn = d_get_child(pn, 1);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".pkParAssign");
    return 1;
  }
  return 0;
}

int mlxtran_pk_process_setStatement(const char* name, D_ParseNode *pn, int i) {
  if (i == 0 &&
      (!strcmp("cmtLine", name) ||
       !strcmp("peripLine", name) ||
       !strcmp("effectLine", name) ||
       !strcmp("transferLine", name) ||
       !strcmp("depotLine", name) ||
       !strcmp("absorptionLine", name) ||
       !strcmp("ivLine", name) ||
       !strcmp("emptyLine", name) ||
       !strcmp("resetLine", name) ||
       !strcmp("eliminationLine", name))) {
    D_ParseNode *xpn = d_get_child(pn, 1);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".pkSetStatement");
    return 1;
  }
  return 0;
}

void wprint_parsetree_mlxtran_pk(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  int nch = d_get_number_of_children(pn);
  if (mlxtran_pk_process_declarePars(name, pn) ||
      mlxtran_pk_process_eqExpr(name, pn)) {
    // return early; no need to process more
    return;
  }
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      if (mlxtran_pk_process_pkmodel2(name, pn, i) ||
          mlxtran_pk_process_pkmodel1(name, pn, i) ||
          mlxtran_pk_process_setStatement(name, pn, i)) {
        continue;
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_mlxtran_pk(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_mlxtran_pk(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_mlxtranPk, sizeof(D_ParseNode_User));
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
    wprint_parsetree_mlxtran_pk(parser_tables_mlxtranPk , _pn, 0, wprint_node_mlxtran_pk, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_mlxtran_pk(SEXP in, SEXP what) {
  sClear(&curLine);
  sClear(&firstErr);
  record = "<MODEL> [LONGITUDINAL] PK:";
  trans_mlxtran_pk(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
