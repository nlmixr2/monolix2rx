//loop
statement_list :
        (statement)+ ;

inpId: identifier;
input1Line: 'input' '=' inpId;
inputLine: 'input' '=' '{' inpId (',' inpId)* '}';
catId: identifier | char_t1 | char_t2 | number | "[^ '\",\t\n}]+";
catCov: identifier '=' '{' 'type' '=' 'categorical' ',' 'categories' '=' '{' catId (',' catId)*   '}' '}';
regressorLine: identifier '=' '{' 'use' '=' 'regressor' '}';

char_t1: "\'([^\'\\]|\\[^])*\'";
char_t2: "\"([^\"\\]|\\[^])*\"";

filename: filename_t1 | filename_t2 | filename_t3 | filename_t4;
filename_t1: "\'([^\'\\]|\\[^])*\'";
filename_t2: "\"([^\"\\]|\\[^])*\"";
filename_t3: "[^ '\"\n]+";
filename_t4: ("[^ .\n]+")+ '.'  "[A-Za-z0-9_]+";

// Monolix 2024 writes the model file as file={path='model.txt'}, which is
// equivalent to the older file='model.txt'.  Monolix quotes the path; an
// unquoted one is swallowed whole by filename_t3 (which allows braces, as
// it always has), so file={path=model.txt} is not supported.
fileLine: 'file' '=' filename
    | 'file' '=' '{' 'path' '=' filename '}';

statement: inputLine singleLineComment?
    | input1Line  singleLineComment?
    | catCov singleLineComment?
    | regressorLine singleLineComment?
    | fileLine singleLineComment?
    ;

number: ('+' | '-')? constant;
constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
