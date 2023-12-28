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
#include "equation.g.d_parser.h"
#define max2( a , b )  ( (a) > (b) ? (a) : (b) )

extern D_ParserTables parser_tables_equation;
#define curP monolix2rx_equation_curP

#define gBuf monolix2rx_equation_gBuf
#define gBufFree monolix2rx_equation_gBufFree
#define gBufLast monolix2rx_equation_gBufLast
#define _pn monolix2rx_equation__pn
#define freeP monolix2rx_equation_freeP
#define parseFreeLast monolix2rx_equation_parseFreeLast
#define parseFree monolix2rx_equation_parseFree
#define errP monolix2rx_equation_errP
#include "parseSyntaxErrors.h"
#include "util.h"

char* gBuf;
int gBufLast = 0;
int gBufFree = 0;
D_Parser *curP=NULL;
D_Parser *errP=NULL;
D_ParseNode *_pn = 0;
extern sbuf sbTransErr;

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
typedef void (print_node_fn_t)(int depth, char *token_name, char *token_value, void *client_equation);
void wprint_node_equation(int depth, char *token_name, char *token_value, void *client_equation) {}

extern sbuf curLine;

void pushModel(void) {
  if (curLine.s == NULL) return;
  if (curLine.s[0] == 0) return;
  monolix2rxSingle(curLine.s, ".equationLine");
  sClear(&curLine);
}

int equation_operators(const char *name, D_ParseNode *pn) {
  if (!strcmp("(", name) ||
      !strcmp(")", name)) {
    sAppend(&curLine, "%s", name);
    return 1;
  } else if (!strcmp("*", name) ||
             !strcmp("/", name) ||
             !strcmp("+", name) ||
             !strcmp("-", name)) {
    sAppend(&curLine, " %s ", name);
    return 1;
  } else if (!strcmp(",", name)) {
    sAppendN(&curLine, ", ", 2);
  }
  if (!strcmp("^", name)) {
    sAppendN(&curLine, "^", 1);
    return 1;
  }
  if (!strcmp("=", name)) {
    sAppendN(&curLine, " <- ", 4);
    return 1;
  }
  return 0;
}


int equation_logic_operators(const char *name, D_ParseNode *pn) {
  if (!strcmp("le_expression_monolix", name)) {
    sAppendN(&curLine, " <= ", 4);
    return 1;
  } else if (!strcmp("ge_expression_monolix", name)) {
    sAppendN(&curLine, " >= ", 4);
    return 1;
  } else if (!strcmp("gt_expression_monolix", name)) {
    sAppendN(&curLine, " > ", 3);
    return 1;
  } else if (!strcmp("lt_expression_monolix", name)) {
    sAppendN(&curLine, " < ", 3);
    return 1;
  } else if (!strcmp("neq_expression_monolix", name)) {
    sAppendN(&curLine, " != ", 4);
    return 1;
  } else if (!strcmp("eq_expression_monolix", name)) {
    sAppendN(&curLine, " == ", 4);
    return 1;
  } else if (!strcmp("and_expression_monolix", name)) {
    sAppendN(&curLine, " && ", 4);
    return 1;
  } else if (!strcmp("or_expression_monolix", name)) {
    sAppendN(&curLine, " || ", 4);
    return 1;
  }
  return 0;
}

int equation_identifier_or_constant(char *name,  D_ParseNode *pn) {
  if (!strcmp("identifier", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (!strcmp("amtDose", v)) {
      sAppendN(&curLine, "dose", 4);
      return 1;
    } else if (!strcmp("inftDose", v)) {
      sClear(&sbTransErr);
      sAppend(&sbTransErr, "'inftDose' Monolix declaration not supported in translation");
      updateSyntaxCol();
      trans_syntax_error_report_fn0(sbTransErr.s);
      finalizeSyntaxError();
      return 1;
    } else if (!strcmp("tDose", v)) {
      sAppendN(&curLine, "tlast", 5);
      return 1;
    } else if (!strcmp("t", v)) {
      sAppendN(&curLine, "time", 4);
      return 1;
    } else if (!strcmp("t_0", v) || !strcmp("t0", v)) {
      sClear(&sbTransErr);
      sAppend(&sbTransErr, "'t_0' or 't0' Monolix declaration not supported in translation");
      updateSyntaxCol();
      trans_syntax_error_report_fn0(sbTransErr.s);
      finalizeSyntaxError();
      return 1;
    }
    // ddt_x becomes d/dt(x)
    char *v2 = v;
    if (v2[0] == 'd' && v2++ &&
        v2[0] == 'd' && v2++ &&
        v2[0] == 't' && v2++ &&
        v2[0] == '_') {
      sAppend(&curLine, "d/dt(%s)", v2+1);
      return 1;
    }
    v2 = v;
    // x_0 becomes x(0)
    while (v2[0] != 0 && v2[0] != '_') {
      v2++;
    }
    if (v2[0] == '_' && !strcmp(v2+1, "0")) {
      v2[0] = 0;
      sAppend(&curLine, "%s(0)", v);
      return 1;
    }
    sAppend(&curLine, "%s", v);
    return 1;
  } else if (!strcmp("constant", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    sAppend(&curLine, "%s", v);
    return 1;
  }
  return 0;
}

int equation_function_name(char *name,  D_ParseNode *pn) {
  if (!strcmp("function1_name", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (!strcmp("invlogit(", v)) {
      sAppendN(&curLine, "expit(", 6);
      return 1;
    } else if (!strcmp("norminv(", v)) {
      sAppendN(&curLine, "qnorm(", 6);
      return 1;
    } else if (!strcmp("normcdf(", v)) {
      sAppendN(&curLine, "pnorm(", 6);
      return 1;
    } else if (!strcmp("gammaln(", v)) {
      sAppendN(&curLine, "lgamma(", 7);
      return 1;
    } else if (!strcmp("factln(", v)) {
      sAppendN(&curLine, "lfactorial(", 11);
      return 1;
    } else if (!strcmp("rem(", v)) {
      sClear(&sbTransErr);
      sAppend(&sbTransErr, "rem() not supported in translation");
      updateSyntaxCol();
      trans_syntax_error_report_fn0(sbTransErr.s);
      finalizeSyntaxError();
      return 1;
    }
    sAppend(&curLine, "%s", v);
    return 1;
  } else if (!strcmp("function2_name", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    if (!strcmp("delay(", v)) {
      sClear(&sbTransErr);
      sAppend(&sbTransErr, "delay() not supported in translation");
      updateSyntaxCol();
      trans_syntax_error_report_fn0(sbTransErr.s);
      finalizeSyntaxError();
      return 1;
    }
    sAppend(&curLine, "%s", v);
    return 1;
  }
  return 0;
}

int equation_if(char *name,  D_ParseNode *pn, int i) {
  if (i == 0) {
    if (!strcmp("if", name)) {
      sAppendN(&curLine, "if (", 4);
      return 2;
    } else if (!strcmp("elseif", name)) {
      sAppendN(&curLine, "} else if (", 11);
      return 2;
    } else if (!strcmp("else", name)) {
      sAppendN(&curLine, "} else {", 8);
      return 1;
    } else if (!strcmp("endit", name)) {
      sAppendN(&curLine, "}", 1);
      return 1;
    }
  }
  return 0;
}

int equation_handle_odeType(char *name,  D_ParseNode *pn) {
  if (!strcmp(name, "odeType")) {
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".equationOdeType");
    return 1;
  }
  return 0;
}

void wprint_parsetree_equation(D_ParserTables pt, D_ParseNode *pn, int depth, print_node_fn_t fn, void *client_data) {
  char *name = (char*)pt.symbols[pn->symbol].name;
  if (equation_operators(name, pn) ||
      equation_logic_operators(name, pn) ||
      equation_identifier_or_constant(name, pn) ||
      equation_function_name(name,  pn) ||
      equation_handle_odeType(name, pn)) {
    return;
  }
  int nch = d_get_number_of_children(pn);
  if (nch != 0) {
    int needEnd=0, needEnd2=0;
    for (int i = 0; i < nch; i++) {
      needEnd2 =equation_if(name,  pn, i);
      if (needEnd2) {
        needEnd = needEnd2;
        continue;
      }
      D_ParseNode *xpn = d_get_child(pn, i);
      // process other arguments
      wprint_parsetree_equation(pt, xpn, depth, fn, client_data);
    }
    if (needEnd == 2) {
      sAppendN(&curLine, ") {", 3);
      pushModel();
    } else if (needEnd == 1) {
      pushModel();
    } else if (!strcmp("assignment", name)) {
      pushModel();
    } else if (!strcmp("odeType", name)) {
      sClear(&curLine);
    }
  }
}

void trans_equation(const char* parse){
  freeP();
  curP = new_D_Parser(&parser_tables_equation, sizeof(D_ParseNode_User));
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
    wprint_parsetree_equation(parser_tables_equation , _pn, 0, wprint_node_equation, NULL);
  }
  finalizeSyntaxError();
}

SEXP _monolix2rx_trans_equation(SEXP in) {
  sClear(&curLine);
  sClear(&firstErr);
  record = "[LONGITUDINAL] EQUATION:";
  trans_equation(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
