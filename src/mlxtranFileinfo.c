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
#include "mlxtranFileinfo.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_mlxtranFileinfo;
#define curP monolix2rx_fileinfo_curP

#define gBuf monolix2rx_fileinfo_gBuf
#define gBufFree monolix2rx_fileinfo_gBufFree
#define gBufLast monolix2rx_fileinfo_gBufLast
#define _pn monolix2rx_fileinfo__pn
#define freeP monolix2rx_fileinfo_freeP
#define parseFreeLast monolix2rx_fileinfo_parseFreeLast
#define parseFree monolix2rx_fileinfo_parseFree
#define errP monolix2rx_fileinfo_errP
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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_fileinfo);
void wprint_node_fileinfo(int depth, char *token_name, char *token_value, void *client_fileinfo) {}

extern sbuf curLine;

int fileinfo_process_delimiter(const char *name, D_ParseNode *pn) {
  if (!strcmp("delimiterType", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".fileinfoDelimiter");
    return 1;
  }
  return 0;
}

void wprint_parsetree_fileinfo(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  int nch = d_get_number_of_children(pn);
  if (fileinfo_process_delimiter(name, pn)) {
    return;
  } else if (!strcmp("filename_t3", name) ||
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
  } else if (!strcmp("identifier", name)) {
    char *v = (char*)rc_dup_str(pn->start_loc.s, pn->end);
    monolix2rxFileinfoHeader(v);
  }
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_fileinfo(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_fileinfo(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_mlxtranFileinfo, sizeof(D_ParseNode_User));
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
    wprint_parsetree_fileinfo(parser_tables_mlxtranFileinfo , _pn, 0, wprint_node_fileinfo, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_fileinfo(SEXP in) {
  sClear(&curLine);
  sClear(&firstErr);
  record = "[FILEINFO]";
  trans_fileinfo(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
