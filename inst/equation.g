//loop
statement_list :
        (statement)+ ;

odeType: 'odeType' '=' ('stiff' | 'nonStiff' );

statement: odeType singleLineComment?
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

or_expression_monolix: '||' | '|';

and_expression_monolix: '&' | '&&';

equality_expression0 : equality_expression |
        '(' equality_expression ')';

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

logical_and_expression : equality_expression0
        (and_expression_monolix equality_expression0)* ;

logical_or_expression : logical_and_expression
        (or_expression_monolix  logical_and_expression)* ;

additive_expression : multiplicative_expression
        (('+' | '-') multiplicative_expression)* ;

multiplicative_expression : unary_expression
        (mult_part)* ;

mult_part : ('*' | '/') unary_expression ;

unary_expression : ('+' | '-')? (primary_expression | power_expression);

exponent_expression : ('+' | '-')? (primary_expression | power_expression);

power_expression : primary_expression power_operator exponent_expression;

power_operator   : '^';

function: function1 | function2;

function2_name: 'atan2(' | 'min(' | 'max(';

function2 : function2_name (logical_or_expression)*  ',' logical_or_expression* ')' ;

function1 : function1_name (logical_or_expression)* ')' ;
function1_name: 'abs(' | 'sqrt(' | 'exp(' | 'log(' | 'log10(' | 'logit(' |
        'invlogit(' | 'probit(' | 'norminv(' | 'qnorm(' | 'normcdf(' |
        'pnorm(' | 'sin(' | 'cos(' | 'tan(' |  'asin(' |  'acos(' |
        'atan(' |  'cosh(' | 'tanh(' | 'gammaln(' | 'lgamma(' |
        'floor(' | 'ceil(' | 'factorial(' | 'factln(' | 'rem(' |
        'delay(';


constant : decimalint | float1 | float2;

primary_expression
    : constant
    | identifier
    | function
    | '(' logical_or_expression ')'
    ;

decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
