//loop
statement_list :
        (statement)+ ;

filename: filename_t1 | filename_t2 | filename_t3 | filename_t4;
filename_t1: "\'([^\'\\]|\\[^])*\'";
filename_t2: "\"([^\"\\]|\\[^])*\"";
filename_t3: "[^ '\"\n]+";
filename_t4: ("[^ .\n]+")+ '.'  "[A-Za-z0-9_]+";

fileLine: 'file' '=' filename;
delimiterType: ('comma' |'tab' | 'space' | 'semicolon');
delimiterLine: 'delimiter' '=' delimiterType;
headerLine: 'header' '=' '{' identifier (',' identifier)* '}';



statement: fileLine singleLineComment?
    | delimiterLine singleLineComment?
    | headerLine singleLineComment?
    ;
constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
