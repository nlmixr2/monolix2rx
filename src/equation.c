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
int gIsAssignmentStart = 0;
char *curDdt;
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
  if (!strcmp(curLine.s, " <- ")) {
    sClear(&curLine);
    return;
  }
  monolix2rxDouble(curLine.s, curDdt, ".equationLine");
  // now check for X_0 = which will be followed by X(0) = X_0
  char *v2 = curLine.s;
  while (v2[0] != 0 && v2[0] != '_' && v2[0] != '=') {
    v2++;
  }
  if (v2[0] == '_' && v2[1] == '0' && (v2[2] == ' ' || v2[2] == '=')) {
    v2[0] = 0;
    char *v = (char*)rc_dup_str(curLine.s, v2);
    sClear(&curLine);
    sAppend(&curLine, "%s(0) <- %s_0", v, v);
    monolix2rxDouble(curLine.s, curDdt, ".equationLine");
  }
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
    }
    // ddt_x becomes d/dt(x)
    char *v2 = v;
    if (v2[0] == 'd' && v2++ &&
        v2[0] == 'd' && v2++ &&
        v2[0] == 't' && v2++ &&
        v2[0] == '_') {
      monolix2rxSingle(v2+1, ".equationState");
      sAppend(&curLine, "d/dt(%s)", v2+1);
      if (gIsAssignmentStart) {
        monolix2rxSingle(v2+1, ".equationLhs");
      } else {
        monolix2rxSingle(v2+1, ".equationRhs");
      }
      if (gIsAssignmentStart) {
        curDdt = (char*)rc_dup_str(v2+1, v2+1+strlen(v2+1));
      }
      return 1;
    }
    v2 = v;
    if (gIsAssignmentStart) {
      monolix2rxSingle(v, ".equationLhs");
    } else {
      monolix2rxSingle(v, ".equationRhs");
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
  } else if (!strcmp("bsmm_fun", name)) {
    sClear(&sbTransErr);
    sAppend(&sbTransErr, "bsmm() not supported in translation");
    updateSyntaxCol();
    trans_syntax_error_report_fn0(sbTransErr.s);
    finalizeSyntaxError();
    return 1;
  } else if (!strcmp("wsmm_fun", name)) {
    sClear(&sbTransErr);
    sAppend(&sbTransErr, "wsmm() not supported in translation");
    updateSyntaxCol();
    trans_syntax_error_report_fn0(sbTransErr.s);
    finalizeSyntaxError();
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

int mlxtran_pk_process_pkmodel1(const char* name, D_ParseNode *pn, int i) {
  if (!strcmp("pkmodel1", name)) {
    if (i == 0) {
      D_ParseNode *xpn = d_get_child(pn, 0);
      char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
      monolix2rxSingle(v, ".pkSetCc");
      monolix2rxSingle("pkmodel", ".pkSetStatement");
      return 1;
    } else if (i == 1 || i == 2) {
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
    } else if (i == 1 || i == 2 || i == 3 || i == 4 || i == 5 || i == 6) {
      return 1;
    }
  }
  return 0;
}

int mlxtran_pk_process_declarePars(const char* name, D_ParseNode *pn, int i) {
  if (i == 0 &&
      (!strcmp("pkpars", name) ||
       !strcmp("pkparsE", name) ||
       !strcmp("ke0Op", name) ||
       !strcmp("TlagOp", name) ||
       !strcmp("pOp", name) ||
       !strcmp("Tk0Op", name) ||
       !strcmp("kaOp", name) ||
       !strcmp("KtrOp", name) ||
       !strcmp("ktOp", name) ||
       !strcmp("MttOp", name) ||
       !strcmp("kOp", name) ||
       !strcmp("clOp", name) ||
       !strcmp("vmOp", name) ||
       !strcmp("kmOp", name))) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    // check for '=' and call par declare followed by assign if found.
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    char *v2 = v;
    while (v2[0] != 0 && v2[0] != '=') {
      v2++;
    }
    if (v2[0] == '=') {
      // This argument was somehow missed by the parser
      v2[0] = 0;
      monolix2rxSingle(v, ".pkParDeclare");
      // take off white space for second argument
      v2++;
      while (v2[0] == ' ' || v2[0] == '\t') {
        v2++;
      }
      v = v2;
      while (v[0] != 0) {
        v++;
      }
      v--;
      while (v[0] == ' ' || v[0] == '\t') {
        v[0] = 0;
        v--;
      }
      monolix2rxSingle(v2, ".pkParAssign");
    } else {
      monolix2rxSingle(v, ".pkParDeclare");
    }
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

int mlxtran_pk_process_strict_ops(const char *name, D_ParseNode *pn) {
  // option = value types
  if (!strcmp("cmtOp", name) ||
      !strcmp("amtOp", name) ||
      !strcmp("vOp", name) ||
      !strcmp("cpOp", name) ||
      !strcmp("fromOp", name) ||
      !strcmp("toOp", name) ||
      !strcmp("targetOp", name)) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".pkParDeclare");
    xpn = d_get_child(pn, 2);
    v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".pkParAssign");
    return 1;
  } else if (!strcmp("admOp", name)) {
    monolix2rxSingle("adm", ".pkParDeclare");
    D_ParseNode *xpn = d_get_child(pn, 2);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".pkParAssign");
    return 1;
  }
  return 0;
}

int mlxtran_pk_process_knum(const char *name, D_ParseNode *pn) {
  if (!strcmp(name, "kNN") ||
      !strcmp(name, "kN_N")) {
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s+1, xpn->end);
    monolix2rxSingle(v, ".pkSetK");
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
    D_ParseNode *xpn = d_get_child(pn, 0);
    char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
    monolix2rxSingle(v, ".pkSetStatement");
    return 1;
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
      equation_handle_odeType(name, pn) ||
      mlxtran_pk_process_strict_ops(name, pn) ||
      mlxtran_pk_process_eqExpr(name, pn) ||
      mlxtran_pk_process_knum(name, pn)) {
    return;
  }
  int nch = d_get_number_of_children(pn);
  if (nch != 0) {
    int needEnd=0, needEnd2=0;
    for (int i = 0; i < nch; i++) {
      if (mlxtran_pk_process_pkmodel2(name, pn, i) ||
          mlxtran_pk_process_pkmodel1(name, pn, i) ||
          mlxtran_pk_process_setStatement(name, pn, i) ||
          mlxtran_pk_process_declarePars(name, pn, i)) {
        continue;
      }
      if (i == 0 && !strcmp("assignment", name)) {
        sClear(&curLine);
        const char *none="";
        curDdt=rc_dup_str(none, none);
        D_ParseNode *xpn = d_get_child(pn, 0);
        char *v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
        if (!strcmp(v, "t_0") || !strcmp(v, "t0")) {
          xpn = d_get_child(pn, 2);
          v = (char*)rc_dup_str(xpn->start_loc.s, xpn->end);
          char *v2 = v;
          while (v2[0] == ' ' || v2[0] == '0' || v2[0] == '.' || v2[0] == '\t' || v2[0] == '\n') {
            v2++;
          }
          if (v2[0] != 0) {
            Rf_warning("%s 't_0' or 't0' are assigned to a non-zero value (which is unsupported by rxode2), ignoring",
                       v2);
          }
          return;
        }
        gIsAssignmentStart=1;
      } else if (!strcmp("assignment", name)) {
        gIsAssignmentStart=0;
      }
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

SEXP _monolix2rx_trans_equation(SEXP in, SEXP what) {
  sClear(&curLine);
  sClear(&firstErr);
  record = R_CHAR(STRING_ELT(what, 0));
  trans_equation(R_CHAR(STRING_ELT(in, 0)));
  parseFree(0);
  return R_NilValue;
}
