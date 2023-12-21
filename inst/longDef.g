//loop
statement_list :
        (statement)+ ;

predOp: 'prediction' '=' identifier;

distType: 'logNormal' | 'logitNormal' | 'normal' | 'lognormal' | 'logitnormal';
distOp: 'distribution' '=' distType;

errPar: constant | identifier;

combined1: 'combined1' '(' errPar ',' errPar ')';
combined2: 'combined2' '(' errPar ',' errPar ')';

combined1c: 'combined1c' '(' errPar ',' errPar ',' errPar ')';
combined2c: 'combined2c' '(' errPar ',' errPar ',' errPar ')';

constant: 'constant' '(' errPar ')';
proportional: 'proportional' '(' errPar ')';

errModels: proportional
         | constant
         | combined1
         | combined2
         | combined1c
         | combined2c
         ;
errOp: 'errorModel' '=' errModels;

endpointOp: distOp | errOp | predOp;
endpoint: identifier '=' '{' endpointOp (',' endpointOp)*'}';

statement: endpoint singleLineComment?
    ;

constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
