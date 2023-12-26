//loop
statement_list :
        (statement)+ ;


pkpars0: 'V' | 'Tk0' | 'ka' | 'Ktr' | 'Mtt' | 'Tlag' | 'p'
    |  'k' | 'Cl' | 'Vm' | 'Km' | 'k12' | 'k21' |  'k13'
    | 'k31';

pkparsE0:  pkpars | 'ke0';

eqExpr: '=' "[^\n,)]*";

pkpars: pkpars0 eqExpr?;

pkparsE: pkparsE0 eqExpr?;

pkmodel1: identifier '=' 'pkmodel' '(' pkpars (',' pkpars)* ')';
pkmodel2: '{' identifier ',' identifier '}' '=' 'pkmodel' '(' pkparsE (',' pkparsE)* ')';

cmtOp: 'cmt' '=' decimalint;
amtOp: 'amount' '=' identifier;
vOp:   'volume' '=' (identifier | constant);
cpOp:  'concentration' '=' identifier;
cmtOps: cmtOp | amtOp | vOp | cpOp;
cmtLine: 'compartment'  '(' cmtOps (',' cmtOps)* ')';

kNN: "k[1-9][1-9]";
kN_N: "k[1-9][0-9]*_[1-9][0-9]*";
kP: kNN eqExpr? | kN_N eqExpr?;

peripOp: kP | amtOp | vOp | cpOp;
peripLine: 'peripheral'  '(' peripOp (',' peripOp)* ')';
ke0Op: 'ke0' eqExpr?;
effOp: cmtOp | cpOp | ke0Op;
effectLine: 'effect'  '(' effOp (',' effOp)* ')';

fromOp: 'from' '=' decimalint;
toOp: 'to' '=' decimalint;
ktrOp: 'ktr' eqExpr?;
transferOps: fromOp | toOp | ktrOp;
transferLine: 'transfer' '(' transferOps (',' transferOps)* ')';

admOp: ('adm' | 'type') '=' decimalint;
targetOp: 'target'  '=' identifier;

TlagOp: 'Tlag' eqExpr?;
pOp: 'p' eqExpr?;
Tk0Op: 'Tk0' eqExpr?;
kaOp: 'ka' eqExpr?;
KtrOp: 'Ktr' eqExpr?;
MttOp: 'Mtt' eqExpr?;

depotOps: admOp | targetOp | TlagOp | pOp | Tk0Op | kaOp | KtrOp |  MttOp;
depotLine: 'depot' '(' depotOps (',' depotOps) ')';


absOrOral: ('absorption' | 'oral');

absOps: admOp | TlagOp | pOp | cmtOp | Tk0Op | kaOp | KtrOp | MttOp;

absorptionLine: absOrOral '(' absOps (',' absOps) ')';


ivOps: cmtOp | admOp | TlagOp | pOp;
ivLine: 'iv' '(' ivOps (',' ivOps)* ')';

targetOp: 'target' '=' identifier;
emptyOp: admOp | targetOp;
emptyLine: 'empty' '(' emptyOp (',' emptyOp)* ')';
resetLine: 'reset' '(' emptyOp (',' emptyOp)* ')';

kOp: 'k' eqExpr?;
clOp: 'Cl' eqExpr?;
vmOp: 'Vm' eqExpr?;
kmOp: 'Km' eqExpr?;
eliminationOp: cmtOp | vOp | kOp | clOp | vmOp | kmOp;
eliminationLine: 'elimination' '(' eliminationOp (',' eliminationOp)* ')';
pLine:  identifier '=' "[^\n},;]*";
codeLine: pLine+;
statement: pkmodel1 singleLineComment?
    | pkmodel2 singleLineComment?
    | cmtLine singleLineComment?
    | peripLine singleLineComment?
    | transferLine singleLineComment?
    | depotLine  singleLineComment?
    | absorptionLine  singleLineComment?
    | ivLine singleLineComment?
    | emptyLine singleLineComment?
    | resetLine singleLineComment?
    | eliminationLine singleLineComment?
    | codeLine singleLineComment?
    ;

constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
