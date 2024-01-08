//loop
statement_list :
        (statement)+ ;

logitNormalDist: 'distribution' '=' ('logitNormal' | 'logitnormal');

distribution: 'distribution' '=' ('normal' | 'logNormal' | 'lognormal' | 'probitnormal' | 'probitNormal');

typicalVar: 'typical' '=' identifier;
meanVar: 'mean' '=' identifier;

typicalFixed: 'typical' '=' number;
meanFixed: 'mean' '=' number;

typicalOption: (typicalVar | typicalFixed | meanVar | meanFixed);

covId: identifier;
covariateSingleOp: 'covariate' '=' covId;
covariateMultOp: 'covariate' '=' '{' covId (',' covId)* '}';
covariateOp: covariateSingleOp | covariateMultOp;

coefItem: identifier | number;
coefItemL: identifier | number;

coeffSingle: 'coefficient' '=' coefItem;
coeffList: '{' coefItemL (',' coefItemL)* '}';
coeffComplex: 'coefficient' '=' '{' (coefItem | coeffList) (',' (coefItem | coeffList))* '}';
coeffOp: coeffSingle | coeffComplex;

sdItem: identifier | number;

sdList: '{' sdItem (',' sdItem)* '}';
sdOp: 'sd' '=' sdItem;
sdLstItem: 'sd' '=' sdList;

varItem: identifier | number;
varOp: 'var' '=' varItem;
varList: '{' varItem (',' varItem)* '}';
varLstItem: 'var' '=' varList;

noVar: 'no-variability';

varOption: (sdOp | sdLstItem | varOp | varLstItem  | noVar);

iovItem: identifier ('*' identifier)* ;
iovItemVL: iovItem;

iovOp1: 'varlevel' '=' '{' iovItemVL (',' iovItemVL)* '}';
iovOp2: 'varlevel' '='  iovItemVL;
iovOp: iovOp1 | iovOp2;

minVal: 'min' '=' number;
maxVal: 'max' '=' number;

otherOptions: typicalOption | varOption | covariateOp | coeffOp | iovOp;
logitOptions: otherOptions | minVal | maxVal ;

logitNormalLine: identifier '=' '{' logitNormalDist (',' logitOptions)* '}';


otherLine: identifier '=' '{' distribution (',' otherOptions)*  '}';
distLine: logitNormalLine | otherLine ;

corrOp: 'r' '(' identifier ',' identifier ')' '=' identifier;

corIovItem: iovItem;

corLine: 'correlation' '=' '{' ('level' '=' corIovItem ',')? corrOp (',' corrOp)* '}';

statement: distLine singleLineComment?
    | corLine singleLineComment?
    ;

number: ('+' | '-')? constant;
constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
