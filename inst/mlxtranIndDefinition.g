//loop
statement_list :
        (statement)+ ;

logitNormalDist: 'distribution' '=' ('logitNormal' | 'logitnormal');

distribution: 'distribution' '=' ('normal' | 'logNormal' | 'lognormal' | 'logitNormal' | 'logitnormal' | 'probitnormal' | 'probitNormal');

typicalVar: 'typical' '=' identifier;
meanVar: 'mean' '=' identifier;

typicalFixed: 'typical' '=' constant;
meanFixed: 'mean' '=' constant;

typicalOption: (typicalVar | typicalFixed | meanVar | meanFixed);

covariateSingleOp: 'covariate' '=' identifier;
covariateMultOp: 'covariate' '=' '{' identifier (',' identifier)* '}';
covariateOp: covariateSingleOp | covariateMultOp;

coefItem: identifier | constant;
coefItemL: identifier | constant;

coeffSingle: 'coefficient' '=' coefItem;
coeffList: '{' coefItemL (',' coefItemL)* '}';
coeffComplex: 'coefficient' '=' '{' (coefItem | coeffList) (',' (coefItem | coeffList))* '}';
coeffOp: coeffSingle | coeffComplex;

sdItem: identifier | constant;

sdList: '{' sdItem (',' sdItem)* '}';
sdOp: 'sd' '=' sdItem;
sdLstItem: 'sd' '=' sdList;

varItem: identifier | constant;
varOp: 'var' '=' varItem;
varList: '{' varOp (',' varOp)* '}';
varLstItem: 'var' '=' varList;

noVar: 'no-variability';

varOption: (sdOp | sdLstItem | varOp | varLstItem | noVar);

iovItem: identifier ('*' identifier)* ;

iovLine: 'varlevel' '=' '{' iovItem (',' iovItem)* '}';

minVal: 'min' '=' constant;
maxVal: 'max' '=' constant;

otherOptions: typicalOption | varOption | covariateOp | coeffOp;
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

constant : decimalint | float1 | float2;
decimalint: "0|([1-9][0-9]*)" $term -1;
float1: "([0-9]+.[0-9]*|[0-9]*.[0-9]+)([eE][\-\+]?[0-9]+)?" $term -2;
float2: "[0-9]+[eE][\-\+]?[0-9]+" $term -3;
identifier: "[a-zA-Z][a-zA-Z0-9_]*" $term -4;
whitespace: ( "[ \t\r\n]+" | singleLineComment )*;
singleLineComment: "[;]" "[^\n]*";
