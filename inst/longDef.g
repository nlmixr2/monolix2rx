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

endpointOp: distOp | errOp | predOp;
endpoint: identifier '=' '{' endpointOp (',' endpointOp)*'}';


eventTypes: 'exact' | 'intervalCensored';
eventTypeOp: 'eventType' '=' eventTypes;
maxEventNumberOp: 'maxEventNumber' '=' decimalint;
rightCensoringTimeOp: 'rightCensoringTime' '=' constant;
intervalLengthOp: 'intervalLength' '=' constant;
hazardOp: 'hazard' '=' identifier;

tteOps: eventTypeOp | maxEventNumberOp | rightCensoringTimeOp | intervalLengthOp | hazardOp;

tte: identifier '=' '{' 'type' '=' 'event' (',' tteOps)* '}';


pIn: identifier ('=' | '<=' | '==' | '>=' | '~=' | '!=') ( identifier | constant);
pOut: 'P(' pIn ('|' pIn)? ')';
pTrans0: 'log(P(' | 'logit(P(' | 'probit(P(';
pTrans: pTrans0 pIn ('|' pIn)? '))';
pFull: pTrans | pOut;

tLine: 'transitionRate(' decimalint ',' decimalint ')';

pLine: (pFull | identifier | tLine ) '=' "[^\n},;]*";
logicLine: ('if' | 'else' | 'end' | 'elseif') "^[\n},;]*";
dependenceLine: 'dependence' '=' 'Markov';
codeLine: (dependenceLine | pLine | logicLine )+;


categoriesInt: decimalint;
categoriesOp: 'categories' '=' '{' categoriesInt (',' categoriesInt)* '}';


catOps: categoriesOp | codeLine;

categorical: identifier '=' '{' 'type' '=' 'categorical' (',' catOps)* '}';

count: identifier '=' '{' 'type' '=' 'count' (',' codeLine)* '}';

statement: endpoint singleLineComment?
    | tte singleLineComment?
    | categorical singleLineComment?
    ;

constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";