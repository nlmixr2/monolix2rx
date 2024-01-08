//loop
statement_list :
        (statement)+ ;

charOp1: identifier '=' identifier;
charOp2: identifier '=' (char_t1 | char_t2);

logicalOp: identifier '=' ('yes' | 'no');
numOp: identifier '=' number;

char_t1: "\'([^\'\\]|\\[^])*\'";
char_t2: "\"([^\"\\]|\\[^])*\"";

statement: charOp1 singleLineComment?
    | charOp2 singleLineComment?
    | logicalOp singleLineComment?
    | numOp singleLineComment?
    ;

number: ('+' | '-')? constant;
constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
