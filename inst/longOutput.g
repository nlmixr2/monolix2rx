//loop
statement_list :
        (statement)+ ;

statement: outputLine singleLineComment?
    | tableLine singleLineComment?
    | tableLine1 singleLineComment?
    | outputLine1 singleLineComment?
    ;


outputItem: identifier;
tableItem: identifier;

outputLine1: 'output' '=' outputItem;
outputLine: 'output' '=' '{' outputItem (',' outputItem)* '}';

tableLine: 'table' '=' '{' tableItem (',' tableItem)* '}';
tableLine1: 'table' '=' tableItem;

float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
