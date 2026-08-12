//loop
statement_list :
        (statement)+ ;

filename: filename_t1 | filename_t2 | filename_t3 | filename_t4;
filename_t1: "\'([^\'\\]|\\[^])*\'";
filename_t2: "\"([^\"\\]|\\[^])*\"";
filename_t3: "[^ '\"\n]+";
filename_t4: ("[^ .\n]+")+ '.'  "[A-Za-z0-9_]+";

// Monolix 2024 writes the data file as file={path='data.csv'}, which is
// equivalent to the older file='data.csv'.  Monolix quotes the path; an
// unquoted one is swallowed whole by filename_t3 (which allows braces, as
// it always has), so file={path=data.csv} is not supported.
fileLine: 'file' '=' filename
    | 'file' '=' '{' 'path' '=' filename '}';
delimiterType: ('comma' |'tab' | 'space' | 'semicolon' | 'semicolumn');
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
