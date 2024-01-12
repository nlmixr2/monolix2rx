//loop
statement_list :
        (statement)+ ;

methodOp: 'method' '=' ('MLE' | 'FIXED' | 'fixed' | 'Fixed' | 'mle' | 'Mle' | 'MAP' | 'Map' | 'map' | 'BAYES' | 'bayes' | 'Bayes');

estLineMlx: identifier '=' '{' 'value' '=' number (',' methodOp)? '}';
estLine2: identifier '=' number;

statement: estLineMlx singleLineComment?
    | estLine2 singleLineComment?
    ;

number: ('-' | '+')? constant;
constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
