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
#include "longDef.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_longDef;
#define curP monolix2rx_longdef_curP

#define gBuf monolix2rx_longdef_gBuf
#define gBufFree monolix2rx_longdef_gBufFree
#define gBufLast monolix2rx_longdef_gBufLast
#define _pn monolix2rx_longdef__pn
#define freeP monolix2rx_longdef_freeP
#define parseFreeLast monolix2rx_longdef_parseFreeLast
#define parseFree monolix2rx_longdef_parseFree
#define errP monolix2rx_longdef_errP
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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_longdef);
void wprint_node_longdef(int depth, char *token_name, char *token_value, void *client_longdef) {}

extern sbuf curLine;

int longdef_process_distOp(const char *name, D_ParseNode *pn) {
  if (!strcmp("distOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSetDist(v);
    return 1;
  }
  return 0;
}

int longdef_process_endpoint(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("endpoint", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefAddEndpoint(v);
    return 1;
  }
  return 0;
}

int longdef_process_predOp(const char *name, D_ParseNode *pn) {
  if (!strcmp("predOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefAddPrediction(v);
    return 1;
  }
  return 0;
}

int longdef_process_combined1(const char *name, D_ParseNode *pn) {
  if (!strcmp("combined1", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v1 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 4);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetCombined1(v1, v2);
    return 1;
  }
  return 0;
}

int longdef_process_combined2(const char *name, D_ParseNode *pn) {
  if (!strcmp("combined2", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v1 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 4);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetCombined2(v1, v2);
    return 1;
  }
  return 0;
}

int longdef_process_combined1c(const char *name, D_ParseNode *pn) {
  if (!strcmp("combined1c", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v1 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 4);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 6);
    char *v3 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetCombined1c(v1, v2, v3);
    return 1;
  }
  return 0;
}

int longdef_process_combined2c(const char *name, D_ParseNode *pn) {
  if (!strcmp("combined2c", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v1 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 4);
    char *v2 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    xpn = d_get_child(pn, 6);
    char *v3 = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetCombined2c(v1, v2, v3);
    return 1;
  }
  return 0;
}

int longdef_process_constantErr(const char *name, D_ParseNode *pn) {
  if (!strcmp("constantErr", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetConstant(v);
    return 1;
  }
  return 0;
}

int longdef_process_proportional(const char *name, D_ParseNode *pn) {
  if (!strcmp("proportional", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetProportional(v);
    return 1;
  }
  return 0;
}

int longdef_process_tte(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("tte", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefAddEndpoint(v);
    monolix2rxSetDist("event");
    return 1;
  }
  return 0;
}

int longdef_process_hazard(const char *name, D_ParseNode *pn) {
  if (!strcmp("hazardOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefAddPrediction(v);
    return 1;
  }
  return 0;
}

int longdef_process_eventType(const char *name, D_ParseNode *pn) {
  if (!strcmp("eventTypes", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetEventType(v);
    return 1;
  }
  return 0;
}

int longdef_process_maxEventNumber(const char *name, D_ParseNode *pn) {
  if (!strcmp("maxEventNumberOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetMaxEventNumber(v);
    return 1;
  }
  return 0;
}

int longdef_process_rightCensoringTime(const char *name, D_ParseNode *pn) {
  if (!strcmp("rightCensoringTimeOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetRightCensoringTime(v);
    return 1;
  }
  return 0;
}

int longdef_process_intervalLength(const char *name, D_ParseNode *pn) {
  if (!strcmp("intervalLengthOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetIntervalLength(v);
    return 1;
  }
  return 0;
}

int longdef_process_categorical(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("categorical", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefAddEndpoint(v);
    monolix2rxSetDist("categorical");
    return 1;
  }
  return 0;
}

int longdef_process_categoriesInt(const char *name, D_ParseNode *pn) {
  if (!strcmp("categoriesInt", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefSetCategoriesInt(v);
    return 1;
  }
  return 0;
}

int longdef_process_count(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("count", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxLongDefAddEndpoint(v);
    monolix2rxSetDist("count");
    return 1;
  }
  return 0;
}

int longdef_process_autocor(const char *name, D_ParseNode *pn) {
  if (!strcmp("autoCorPar", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".addAutocor");
    return 1;
  }
  return 0;
}

int longdef_process_max(const char *name, D_ParseNode *pn) {
  if (!strcmp("maxVal", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".longDefSetMax");
    return 1;
  }
  return 0;
}

int longdef_process_min(const char *name, D_ParseNode *pn) {
  if (!strcmp("minVal", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".longDefSetMin");
    return 1;
  }
  return 0;
}

int longdef_process_allCode(const char *name, D_ParseNode *pn) {
  if (!strcmp("allCode", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".setAllCode");
    return 1;
  }
  return 0;
}

int longdef_process_transformLine(const char *name, D_ParseNode *pn, int i) {
  if (i == 0 && !strcmp("transformLine", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".longDefSetTransformTo");
    return 1;
  }
  return 0;
}
int longdef_process_transformCatDef1(const char *name, D_ParseNode *pn) {
  if (!strcmp("transformCatDef1", name)) {
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformLabel");
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformValue");
    monolix2rxSingleI(0, ".longDefSetTransformB");
    return 1;
  }
  return 0;
}

int longdef_process_transformCatDef2(const char *name, D_ParseNode *pn) {
  if (!strcmp("transformCatDef2", name)) {
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformLabel");
    xpn = d_get_child(pn, 3);
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformValue");
    monolix2rxSingleI(1, ".longDefSetTransformB");
    return 1;
  }
  return 0;
}

int longdef_process_transformCatDef3(const char *name, D_ParseNode *pn, int i) {
  if (!strcmp("transformCatDef3", name)) {
    if (i ==0) {
    } else if (i == 1 || i == 2 || i == 3) {
      return 1;
    } else {
      return 0;
    }
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformLabel");
    xpn = d_get_child(pn, 3);
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformValue");
    monolix2rxSingleI(1, ".longDefSetTransformB");
    return 1;
  }
  return 0;
}

int longdef_process_transform3Val(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "transform3Val")) {
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformValueExtra");
    return 1;
  }
  return 0;
}

int longdef_process_transformOpRef(const char *name, D_ParseNode *pn) {
  if (!strcmp("transformOpRef", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformRef");
    return 1;
  }
  return 0;
}
int longdef_process_transformOpTrans(const char *name, D_ParseNode *pn) {
  if (!strcmp("transformOpTrans", name)) {
    D_ParseNode *xpn = d_get_child(pn, 2);
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
    monolix2rxDoubleI(v, quote, ".longDefSetTransformFrom");
    return 1;
  }
  return 0;
}


void wprint_parsetree_longdef(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  if (longdef_process_distOp(name, pn) ||
      longdef_process_predOp(name, pn) ||
      longdef_process_combined1(name, pn) ||
      longdef_process_combined2(name, pn) ||
      longdef_process_combined1c(name, pn) ||
      longdef_process_combined2c(name, pn) ||
      longdef_process_constantErr(name, pn) ||
      longdef_process_proportional(name, pn) ||
      longdef_process_hazard(name, pn) ||
      longdef_process_eventType(name, pn) ||
      longdef_process_maxEventNumber(name, pn) ||
      longdef_process_rightCensoringTime(name, pn) ||
      longdef_process_intervalLength(name, pn) ||
      longdef_process_categoriesInt(name, pn) ||
      longdef_process_autocor(name, pn) ||
      longdef_process_max(name, pn) ||
      longdef_process_min(name, pn) ||
      longdef_process_allCode(name, pn) ||
      longdef_process_transformCatDef1(name, pn) ||
      longdef_process_transformCatDef2(name, pn) ||
      longdef_process_transformOpRef(name, pn) ||
      longdef_process_transformOpTrans(name, pn) ||
      longdef_process_transform3Val(name, pn)
      ) {
    return;
  }
  int nch = d_get_number_of_children(pn);
  if (nch != 0) {
    for (int i = 0; i < nch; i++) {
      if (longdef_process_endpoint(name, pn, i) ||
          longdef_process_tte(name, pn, i) ||
          longdef_process_categorical(name, pn, i) ||
          longdef_process_count(name, pn, i) ||
          longdef_process_transformLine(name, pn, i) ||
          longdef_process_transformCatDef3(name, pn, i)) {
        continue; // process next args
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_longdef(pt, xpn, depth, fn, client_data);
    }
  }
}

void trans_longdef(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_longDef, sizeof(D_ParseNode_User));
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
    wprint_parsetree_longdef(parser_tables_longDef , _pn, 0, wprint_node_longdef, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_longdef(SEXP in, SEXP wh) {
  sClear(&curLine);
  sClear(&firstErr);
  record = R_CHAR(STRING_ELT(wh, 0));
  trans_longdef(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
