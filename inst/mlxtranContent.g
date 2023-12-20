//loop
statement_list :
        (statement)+ ;

idLine: identifier '=' '{' 'use' '=' 'identifier' '}';
timeLine: identifier '=' '{' 'use' '=' 'time' '}';
evidLine: identifier '=' '{' 'use' '=' 'eventidentifier' '}';
amtLine: identifier '=' '{' 'use' '=' 'amount' '}';
iiLine: identifier '=' '{' 'use' '=' 'interdoseinterval' '}';
censLine: identifier '=' '{' 'use' '=' 'censored' '}';
limitLine: identifier '=' '{' 'use' '=' 'limit' '}';
ytypeLine: identifier '=' '{' 'use' '=' 'observationtype' '}';
admLine: identifier '=' '{' 'use' '=' 'administration' '}';
nbdoses: decimalint;
ssLine: identifier '=' '{' 'use' '=' 'steadystate' (',' 'nbdoses' '=' nbdoses)? '}';
regLine: identifier '=' '{' 'use' '=' 'regressor' '}';
contLine: identifier '=' '{' 'use' '=' 'covariate' ',' 'type' '=' 'continuous' '}';

ynameType: char_t1 | char_t2 | identifier;
ynameOp1: 'yname' '='  ynameType;
ynameOp2: 'yname' '=' '{' ynameType (',' ynameType)* '}';
ynameOp: ynameOp1 | ynameOp2;

nameType: identifier;
nameOp1: 'name' '='  nameType;
nameOp2: 'name' '=' '{' nameType (',' nameType)* '}';
nameOp: nameOp1 | nameOp2;

// FIXME: more types of endpoints
typeOp1: 'type' '=' 'continuous';
typeOp2: 'type' '=' '{' 'continuous' (',' 'continuous')* '}';
typeOp: typeOp1 | typeOp2;

obsOp: ynameOp | nameOp | typeOp;

obsLine: identifier '=' '{' 'use' '=' 'observation' (',' obsOp)* '}';

char_t1: "\'([^\'\\]|\\[^])*\'";
char_t2: "\"([^\"\\]|\\[^])*\"";

catId: identifier | char_t1 | char_t2;
catCov: identifier '=' '{' 'type' '=' 'categorical' ',' 'categories' '=' '{' catId (',' catId)*   '}' '}';

statement: idLine singleLineComment?
    | timeLine singleLineComment?
    | evidLine singleLineComment?
    | amtLine singleLineComment?
    | iiLine singleLineComment?
    | censLine singleLineComment?
    | limitLine singleLineComment?
    | ytypeLine singleLineComment?
    | admLine singleLineComment?
    | ssLine singleLineComment?
    | regLine singleLineComment?
    | contLine singleLineComment?
    | catCov singleLineComment?
    | obsLine singleLineComment?
    ;

constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
