//loop
statement_list :
        (statement)+ ;

addlLine: identifier '=' '{' 'use' '=' 'additionaldose' '}';
idLine: identifier '=' '{' 'use' '=' 'identifier' '}';
timeLine: identifier '=' '{' 'use' '=' 'time' '}';
evidLine: identifier '=' '{' 'use' '=' 'eventidentifier' '}';
amtLine: identifier '=' '{' 'use' '=' 'amount' '}';
iiLine: identifier '=' '{' 'use' '=' 'interdoseinterval' '}';
censLine: identifier '=' '{' 'use' '=' 'censored' '}';
limitLine: identifier '=' '{' 'use' '=' 'limit' '}';
ytypeLine: identifier '=' '{' 'use' '=' 'observationtype' '}';
admLine: identifier '=' '{' 'use' '=' 'administration' '}';
occLine: identifier '=' '{' 'use' '=' 'occasion' '}';
rateLine: identifier '=' '{' 'use' '=' 'rate' '}';
nbd: decimalint;
ssLine: identifier '=' '{' 'use' '=' 'steadystate' (',' 'nbdoses' '=' nbd)? '}';
regressorLine: identifier '=' '{' 'use' '=' 'regressor' '}';
contLine: identifier '=' '{' 'use' '=' 'covariate' ',' 'type' '=' 'continuous' '}';
catLine: identifier '=' '{' 'use' '=' 'covariate' ',' 'type' '=' 'categorical' '}';

ytypeType: char_t1 | char_t2 | identifier | number;
ytypeOp1: 'ytype' '='  ytypeType;
ytypeOp2: 'ytype' '=' '{' ytypeType (',' ytypeType)* '}';
ytypeOp: ytypeOp1 | ytypeOp2;

ynameType: char_t1 | char_t2 | identifier;
ynameOp1: 'yname' '='  ynameType;
ynameOp2: 'yname' '=' '{' ynameType (',' ynameType)* '}';
ynameOp: ynameOp1 | ynameOp2;


nameType: identifier;
nameOp1: 'name' '='  nameType;
nameOp2: 'name' '=' '{' nameType (',' nameType)* '}';
nameOp: nameOp1 | nameOp2;

// FIXME: more types of endpoints
typeVals: 'continuous' | 'event' | 'categorical' | 'count' | 'discrete';
typeOp1: 'type' '=' typeVals;
typeOp2: 'type' '=' '{' typeVals (',' typeVals)* '}';
typeOp: typeOp1 | typeOp2;

obsOp: ynameOp | nameOp | typeOp | ytypeOp;

obsLine: identifier '=' '{' 'use' '=' 'observation' (',' obsOp)* '}';

char_t1: "\'([^\'\\]|\\[^])*\'";
char_t2: "\"([^\"\\]|\\[^])*\"";

catId: identifier | char_t1 | char_t2;
catCov: identifier '=' '{' 'type' '=' 'categorical' ',' 'categories' '=' '{' catId (',' catId)*   '}' '}';

statement: idLine singleLineComment?
    | addlLine singleLineComment?
    | timeLine singleLineComment?
    | evidLine singleLineComment?
    | amtLine singleLineComment?
    | iiLine singleLineComment?
    | censLine singleLineComment?
    | limitLine singleLineComment?
    | ytypeLine singleLineComment?
    | admLine singleLineComment?
    | occLine singleLineComment?
    | rateLine singleLineComment?
    | ssLine singleLineComment?
    | regressorLine singleLineComment?
    | contLine singleLineComment?
    | catLine singleLineComment?
    | catCov singleLineComment?
    | obsLine singleLineComment?
    ;

number: ('+' | '-')? constant;
constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
