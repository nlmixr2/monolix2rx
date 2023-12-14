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

coeffSingle: 'coefficient' '=' (identifier | constant);
coeffList: '{' (identifier | constant) (',' (identifier | constant)) '}';
coeffComplex: 'coefficient' '=' '{' (identifier | constant | coeffList) (',' (identifier | constant | coeffList))* '}';
coeffOp: coeffSingle | coeffComplex;

sdFixed: 'sd' '=' constant;
sdVar: 'sd' '=' identifier;

varFixed: 'var' '=' constant;
varVar: 'var' '=' identifier;
noVar: 'no-variability';

varOption: (sdVar | sdFixed | varFixed | varVar | noVar);

minVal: 'min' '=' constant;
maxVal: 'max' '=' constant;

otherOptions: typicalOption | varOption | covariateOp | coeffOp;
logitOptions: otherOptions | minVal | maxVal ;

logitNormalLine: identifier '=' '{' logitNormalDist (',' logitOptions)* '}';


otherLine: identifier '=' '{' distribution (',' otherOptions)*  '}';
distLine: logitNormalLine | otherLine ;

corrOp: 'r' '(' identifier ',' identifier ')' '=' identifier;

corLine: 'correlation' '=' '{' corrOp (',' corrOp)* '}';

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
