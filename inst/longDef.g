//loop
statement_list :
        (statement)+ ;

predOp: 'prediction' '=' identifier;

distType: 'logNormal' | 'logitNormal' | 'normal' | 'lognormal' | 'logitnormal';
distOp: 'distribution' '=' distType;

errPar: number | identifier;

combined1: 'combined1' '(' errPar ',' errPar ')';
combined2: 'combined2' '(' errPar ',' errPar ')';

combined1c: 'combined1c' '(' errPar ',' errPar ',' errPar ')';
combined2c: 'combined2c' '(' errPar ',' errPar ',' errPar ')';

constantErr: 'constant' '(' errPar ')';
proportional: 'proportional' '(' errPar ')';

errModels: proportional
         | constantErr
         | combined1
         | combined2
         | combined1c
         | combined2c
         ;
errOp: 'errorModel' '=' errModels;

autoCorPar: number | identifier;
autoCorOp: 'autoCorrCoef' '=' autoCorPar;

minVal: number;
minOp: 'min' '=' minVal;

maxVal: number;
maxOp: 'max' '=' maxVal;


endpointOp: distOp | errOp | predOp | autoCorOp | maxOp | minOp;
endpoint: identifier '=' '{' endpointOp (','? endpointOp)*'}';


eventTypes: 'exact' | 'intervalCensored';
eventTypeOp: 'eventType' '=' eventTypes;
maxEventNumberOp: 'maxEventNumber' '=' decimalint;
rightCensoringTimeOp: 'rightCensoringTime' '=' number;
intervalLengthOp: 'intervalLength' '=' number;
hazardOp: 'hazard' '=' identifier;

tteOps: eventTypeOp | maxEventNumberOp | rightCensoringTimeOp | intervalLengthOp | hazardOp;

tte: identifier '=' '{' 'type' '=' 'event' (','? tteOps)* '}';

categoriesInt: decimalint;
categoriesOp: 'categories' '=' '{' categoriesInt (','? categoriesInt)* '}';


catOps: (','? categoriesOp);

categorical: identifier '=' '{' 'type' '=' 'categorical' (catOps)*  ','? allCode '}';

allCode: "[^}]+";

count: identifier '=' '{' 'type' '=' 'count' ','? allCode '}';

// tSex =
// {
//     transform = sex,
//     categories = {
//         'F' = {'0'},
//         'M' = {'1'}  },
//     reference = 'M'
// }

// or
// tAPGAR =
// {
//     transform = APGAR,
//     categories = {
//         High = {10, 8, 9},
//         Low = {1, 2, 3},
//         Med = {4, 5, 6, 7}  },
//     reference = Med
// }

transformItem: char_t1 | char_t2 | number | identifier;
transformOpTrans: 'transform' '=' transformItem;
transformOpRef: 'reference' '=' transformItem;
transformCatDef1:  transformItem '=' transformItem;
transformCatDef2: transformItem '=' '{' transformItem '}';
transform3Val: transformItem;
transformCatDef3: transformItem '=' '{' transformItem (','? transform3Val)+ '}';
transformCatDef: transformCatDef1 | transformCatDef2 | transformCatDef3;
transformCatOp: 'categories' '=' '{' transformCatDef (','? transformCatDef)* '}';
transformOp: transformOpTrans | transformOpRef | transformCatDef1 | transformCatDef2 | transformCatOp;
transformLine: identifier '=' '{' transformOp (','? transformOp)* '}';

statement: endpoint singleLineComment?
    | tte singleLineComment?
    | categorical singleLineComment?
    | count singleLineComment?
    | transformLine singleLineComment?
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
