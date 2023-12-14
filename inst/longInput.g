//loop
statement_list :
        (statement)+ ;

statement: inputItem singleLineComment?
    | regressorLine singleLineComment?;


inputItem: identifier;
inputLine: 'input' '=' '{' inputItem (',' inputItem)* '}';

regressorLine: identifier '=' '{' 'use' '=' 'regressor' '}';

float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
