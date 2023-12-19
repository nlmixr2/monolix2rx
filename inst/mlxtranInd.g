//loop
statement_list :
        (statement)+ ;

inpId: identifier;
inputLine: 'input' '=' '{' inpId (',' inpId)* '}';
catId: identifier;
catCov: identifier '=' '{' 'type' '=' 'categorical' ',' 'categories' '=' '{' catId (',' catId)*   '}' '}';

statement: inputLine singleLineComment?
    | catCov singleLineComment?
    ;

constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
