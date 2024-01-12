//loop
statement_list :
        (statement)+ ;

// dataType = {'1'=plasma, '2'=plasma}

dataTypeLabel: char_t1 | char_t2 | identifier | number;

dLabel: dataTypeLabel '=' dataTypeLabel;

dataType: 'dataType' '=' '{' dLabel (','? dLabel)* '}';
statement: dataType singleLineComment?
            ;

char_t1: "\'([^\'\\]|\\[^])*\'";
char_t2: "\"([^\"\\]|\\[^])*\"";
number: ('+' | '-')? constant;
constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
