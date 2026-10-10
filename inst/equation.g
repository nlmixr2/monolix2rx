//loop
// plain left recursion: dparser flattens an EBNF (statement)+ one level at
// a time on commit, which is quadratic in the number of statements
statement_list : statement_list statement | statement ;

odeType: 'odeType' '=' ('stiff' | 'nonStiff' );

// Q2/V2/Q3/V3: peripheral clearances and volumes (instead of k12/k21/k13/k31)
pkpars0: 'V' | 'Tk0' | 'ka' | 'Ktr' | 'Mtt' | 'Tlag' | 'p'
    |  'k' | 'Cl' | 'Vm' | 'Km' | 'k12' | 'k21' |  'k13'
    | 'k31' | 'Q2' | 'V2' | 'Q3' | 'V3';

pkparsE0:  pkpars0 | 'ke0';

eqExpr: '=' logical_or_expression;

pkpars: pkpars0 eqExpr?;

pkparsE: pkparsE0 eqExpr?;

pkmodel1: identifier '=' 'pkmodel' '(' pkpars (',' pkpars)* ')';
pkmodel2: '{' identifier ',' identifier '}' '=' 'pkmodel' '(' pkparsE (',' pkparsE)* ')';

cmtOp: 'cmt' '=' decimalint;
amtOp: 'amount' '=' identifier;
vOp:   'volume' '=' logical_or_expression;
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
ktOp: 'kt' eqExpr?;
transferOps: fromOp | toOp | ktOp;
transferLine: 'transfer' '(' transferOps (',' transferOps)* ')';

admOp: ('adm' | 'type') '=' decimalint;
targetOp: 'target'  '=' identifier;

TlagOp: 'Tlag' eqExpr?;
pOp: 'p' eqExpr?;
Tk0Op: 'Tk0' eqExpr?;
kaOp: 'ka' eqExpr?;
MttOp: 'Mtt' eqExpr?;
KtrOp: 'Ktr' eqExpr?;
depotOps: admOp | targetOp | TlagOp | pOp | Tk0Op | kaOp | KtrOp |  MttOp;
depotLine: 'depot' '(' depotOps (',' depotOps)* ')';


absOrOral: ('absorption' | 'oral');

absOps: admOp | TlagOp | pOp | cmtOp | Tk0Op | kaOp | KtrOp | MttOp;

absorptionLine: absOrOral '(' absOps (',' absOps)* ')';


ivOps: cmtOp | admOp | TlagOp | pOp;
ivLine: 'iv' '(' ivOps (',' ivOps)* ')';

emptyOp: admOp | targetOp;
emptyLine: 'empty' '(' emptyOp (',' emptyOp)* ')';
resetLine: 'reset' '(' emptyOp (',' emptyOp)* ')';

kOp: 'k' eqExpr?;
clOp: 'Cl' eqExpr?;
vmOp: 'Vm' eqExpr?;
kmOp: 'Km' eqExpr?;
eliminationOp: cmtOp | vOp | kOp | clOp | vmOp | kmOp;
eliminationLine: 'elimination' '(' eliminationOp (',' eliminationOp)* ')';

statement: pkmodel1 singleLineComment?
    | pkmodel2 singleLineComment?
    | cmtLine singleLineComment?
    | peripLine singleLineComment?
    | transferLine singleLineComment?
    | effectLine singleLineComment?
    | depotLine  singleLineComment?
    | absorptionLine  singleLineComment?
    | ivLine singleLineComment?
    | emptyLine singleLineComment?
    | resetLine singleLineComment?
    | eliminationLine singleLineComment?
    | odeType singleLineComment?
    | assignment singleLineComment?
    | if singleLineComment?
    | else singleLineComment?
    | elseif singleLineComment?
    | endit singleLineComment?
    ;

assignment : identifier  '='  logical_or_expression;

if: 'if' logical_or_expression;

elseif: 'elseif' logical_or_expression;

else: 'else';

endit: 'end';

// Keywords only match whole words, so `else`+`if` is not a parse of `elseif`
// nor `end`+`Time = 1` one of `endTime = 1`.  dparser applies this to every
// terminal reachable from these rules (the condition expressions too); a
// keyword is never longer than an identifier at the same spot.
${declare longest_match if elseif else endit}

or_expression_monolix: '||' | '|';

and_expression_monolix: '&' | '&&';


eq_expression_monolix:  '==';
neq_expression_monolix: '!=' | '~=';

relational_expression : additive_expression
        ((lt_expression_monolix | gt_expression_monolix | le_expression_monolix | ge_expression_monolix) additive_expression)* ;

lt_expression_monolix: '<';
gt_expression_monolix: '>';
ge_expression_monolix: '>=';
le_expression_monolix: '<=';

equality_expression : relational_expression
        ((neq_expression_monolix | eq_expression_monolix ) relational_expression)* ;

logical_and_expression : equality_expression
        (and_expression_monolix equality_expression)* ;

logical_or_expression : logical_and_expression
        (or_expression_monolix  logical_and_expression)* ;

additive_expression : multiplicative_expression
        (('+' | '-') multiplicative_expression)* ;

multiplicative_expression : unary_expression
        (mult_part)* ;

mult_part : ('*' | '/') unary_expression ;

unary_expression : ('+' | '-')? (primary_expression | power_expression) | not_expression;

// Monolix negation: ~a or !a
not_expression : ('!' | '~') unary_expression;

exponent_expression : ('+' | '-')? (primary_expression | power_expression);

power_expression : primary_expression power_operator exponent_expression;

power_operator   : '^';

function: function1 | function2 | rem_fun;

// rem(a, b) is translated to rxode2's a %% b (C fmod, the sign of a)
rem_fun: 'rem(' logical_or_expression ',' logical_or_expression ')';

function2_name: 'atan2(' | 'min(' | 'max(' | 'delay(';

function2 : function2_name logical_or_expression?  ',' logical_or_expression? ')' ;

function1 : function1_name logical_or_expression? ')' ;
function1_name: 'abs(' | 'sqrt(' | 'exp(' | 'log(' | 'log10(' | 'logit(' |
        'invlogit(' | 'probit(' | 'norminv(' | 'qnorm(' | 'normcdf(' |
        'pnorm(' | 'sin(' | 'cos(' | 'tan(' |  'asin(' |  'acos(' |
        'atan(' | 'sinh(' | 'cosh(' | 'tanh(' | 'gammaln(' | 'lgamma(' |
        'floor(' | 'ceil(' | 'factorial(' | 'factln(';

bsmm_item: logical_or_expression;
bsmm_fun: 'bsmm(' bsmm_item  ',' bsmm_item (',' bsmm_item ',' bsmm_item)* ')';

wsmm_item: logical_or_expression;
wsmm_fun: 'wsmm(' wsmm_item  ',' wsmm_item (',' wsmm_item ',' wsmm_item)* ')';

constant : decimalint | float1 | float2;

primary_expression
    : constant
    | identifier
    | function
    | bsmm_fun
    | wsmm_fun
    | '(' logical_or_expression ')'
    ;

number: ('+' | '-' )? constant;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
