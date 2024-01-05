//loop
statement_list :
        (statement)+ ;

char_t1: "\'([^\'\\]|\\[^])*\'";
char_t2: "\"([^\"\\]|\\[^])*\"";

datId: identifier | char_t1 | char_t2;
dataLine: 'data' '=' '{' datId (',' datId)* '}';
dataLine1: 'data' '='  datId;
modelId: identifier | char_t1 | char_t2;
modelLine: 'model' '=' '{' modelId (',' modelId)* '}';
modelLine1: 'model' '=' modelId;

statement: dataLine singleLineComment?
    | modelLine singleLineComment?
    | modelLine1 singleLineComment?
    | dataLine1 singleLineComment?
    ;

constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
