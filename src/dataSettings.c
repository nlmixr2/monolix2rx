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
#include "dataSettings.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_dataSettings;
#define curP monolix2rx_data_settings_curP

#define gBuf monolix2rx_data_settings_gBuf
#define gBufFree monolix2rx_data_settings_gBufFree
#define gBufLast monolix2rx_data_settings_gBufLast
#define _pn monolix2rx_data_settings__pn
#define freeP monolix2rx_data_settings_freeP
#define parseFreeLast monolix2rx_data_settings_parseFreeLast
#define parseFree monolix2rx_data_settings_parseFree
#define errP monolix2rx_data_settings_errP
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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_data_settings);
void wprint_node_data_settings(int depth, char *token_name, char *token_value, void *client_data_settings) {}

extern sbuf curLine;

int data_settings_process_dLabel(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "dLabel")) {
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
    monolix2rxDoubleI(v, quote, ".dataSettingsLabel");
    xpn = d_get_child(pn, 2);
    v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    quote = 0;
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
    monolix2rxDoubleI(v, quote, ".dataSettingsValue");
    return 1;
  }
  return 0;
}

void wprint_parsetree_data_settings(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  if (data_settings_process_dLabel(name, pn)) {
    return;
  }
  int nch = d_get_number_of_children(pn);
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_data_settings(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_data_settings(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_dataSettings, sizeof(D_ParseNode_User));
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
    wprint_parsetree_data_settings(parser_tables_dataSettings , _pn, 0, wprint_node_data_settings, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_data_settings(SEXP in) {
  sClear(&curLine);
  sClear(&firstErr);
  record = "<DATAFILE> [SETTINGS]";
  trans_data_settings(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
