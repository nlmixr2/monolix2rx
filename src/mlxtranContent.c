#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>   /* dj: import intptr_t */
//#include "ode.h"
#include <rxode2parseSbuf.h>
#include <errno.h>
#include "dparser3.h"
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
#include "mlxtranContent.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_mlxtranContent;
#define curP monolix2rx_content_curP

#define gBuf monolix2rx_content_gBuf
#define gBufFree monolix2rx_content_gBufFree
#define gBufLast monolix2rx_content_gBufLast
#define _pn monolix2rx_content__pn
#define freeP monolix2rx_content_freeP
#define parseFreeLast monolix2rx_content_parseFreeLast
#define parseFree monolix2rx_content_parseFree
#define errP monolix2rx_content_errP
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
  freeP();
}
void parseFree(int last) {
  freeP();
  if (last){
    parseFreeLast();
  }
}

extern char * rc_dup_str(const char *s, const char *e);
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_content);
void wprint_node_content(int depth, char *token_name, char *token_value, void *client_content) {}

extern sbuf curLine;

int individual_process_catCov(const char *name, D_ParseNode *pn, int i);
int individual_process_catId(const char *name, D_ParseNode *pn);
int individual_process_regressor(const char *name, D_ParseNode *pn);

int content_process_identifier(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "idLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("identifier", v);
    return 1;
  }
  return 0;
}

int content_process_addl(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "addlLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("additionaldose", v);
    return 1;
  }
  return 0;
}

int content_process_time(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "timeLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("time", v);
    return 1;
  }
  return 0;
}

int content_process_evid(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "evidLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("eventidentifier", v);
    return 1;
  }
  return 0;
}

int content_process_amt(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "amtLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("amount", v);
    return 1;
  }
  return 0;
}

int content_process_ii(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "iiLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("interdoseinterval", v);
    return 1;
  }
  return 0;
}

int content_process_cens(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "censLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("censored", v);
    return 1;
  }
  return 0;
}

int content_process_limit(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "limitLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("limit", v);
    return 1;
  }
  return 0;
}

int content_process_ytype(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "ytypeLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("observationtype", v);
    return 1;
  }
  return 0;
}

int content_process_occ(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "occLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("occasion", v);
    return 1;
  }
  return 0;
}

int content_process_rate(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "rateLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("rate", v);
    return 1;
  }
  return 0;
}

int content_process_adm(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "admLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("administration", v);
    return 1;
  }
  return 0;
}

int content_process_ss(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp(name, "ssLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("steadystate", v);
    return 1;
  }
  return 0;
}

int content_process_nbdoses(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "nbd")) {
    char *v = (char*)rc_dup_str(pn->start_loc.s, pn->end);
    monolix2rxContentSetNbdoses(v);
    return 1;
  }
  return 0;
}

int content_process_cont(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "contLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentContentContCov(v);
    return 1;
  }
  return 0;
}

int content_process_cat(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "catLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".indCat");
    return 1;
  }
  return 0;
}

int content_process_obsVar(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp(name, "obsLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("observation", v);
    return 1;
  }
  return 0;
}

int content_process_yname(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "ynameType")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentContentYname(v);
    return 1;
  }
  return 0;
}

int content_process_name(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "nameType")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentContentName(v);
    return 1;
  }
  return 0;
}

int content_process_mdv(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "mdvLine")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentSetUse1("missingdependentvariable", v);
    return 1;
  }
  return 0;
}

int content_process_ytypeObs(const char *name, D_ParseNode *pn) {
  if (!strcmp("ytypeType", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (v[0] == '\'' || v[0] == '\"') {
      char *v2 = v+1;
      while (v2[0] != 0) {
        v2++;
      }
      v2--;
      v2[0] = 0;
      monolix2rxDoubleI(v+1, 1, ".contentYtype");
    } else {
      monolix2rxDoubleI(v, 0, ".contentYtype");
    }
    return 1;
  }
  return 0;
}

int content_process_type(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "typeVals")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxContentContentType(v);
    return 1;
  }
  return 0;
}

void wprint_parsetree_content(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  int nch = d_get_number_of_children(pn);
  if (individual_process_catId(name, pn) ||
      individual_process_regressor(name, pn) ||
      content_process_identifier(name, pn) ||
      content_process_time(name, pn) ||
      content_process_evid(name, pn) ||
      content_process_amt(name, pn) ||
      content_process_ii(name, pn) ||
      content_process_cens(name, pn) ||
      content_process_limit(name, pn) ||
      content_process_ytype(name, pn) ||
      content_process_adm(name, pn) ||
      content_process_nbdoses(name, pn) ||
      content_process_cont(name, pn) ||
      content_process_yname(name, pn) ||
      content_process_name(name, pn) ||
      content_process_type(name, pn) ||
      content_process_cat(name, pn) ||
      content_process_occ(name, pn) ||
      content_process_rate(name, pn) ||
      content_process_ytypeObs(name, pn) ||
      content_process_addl(name, pn) ||
      content_process_mdv(name, pn)
      ) {
    // return early; no need to process more
    return;
  }
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      if (individual_process_catCov(name, pn, i) ||
          content_process_ss(name, pn, i) ||
          content_process_obsVar(name, pn, i)) {
        // don't process this argument more
        continue;
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_content(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_content(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_mlxtranContent, sizeof(D_ParseNode_User));
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
    wprint_parsetree_content(parser_tables_mlxtranContent , _pn, 0, wprint_node_content, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_content(SEXP in) {
  sClear(&curLine);
  sClear(&firstErr);
  record = "[CONTENT]";
  trans_content(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
