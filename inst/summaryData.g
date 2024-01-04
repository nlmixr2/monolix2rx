//loop
statement_list :
        (statement)+ ;

statement: nind singleLineComment?
    | nobs singleLineComment?
    | ndose singleLineComment?
    ;

nind: 'Number' 'of' 'individuals' ':' decimalint;
nobs: 'Number' 'of' 'observations' '(' "[^)]*" ')' ':' decimalint;
ndose: 'Number' 'of' 'doses' ':' decimalint;

decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
