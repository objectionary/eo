grammar Phi;

// Skip spaces, tabs, newLines
WS  : [ \t\r\n]+ -> skip
    ;

program
    : LCB bindings RCB
    ;

object
    : formation
    | application
    | dispatch
    | termination
    ;

formation
    : LSB bindings RSB
    ;

bindings
    : binding?
    | binding (COMMA binding)*
    ;

binding
    : alphaBinding
    | emptyBinding
    | deltaBidning
    | lambdaBidning
    ;

alphaBinding
    : attribute ARROW object
    ;

attribute
    : PHI
    | RHO
    | SIGMA
    | VTX
    | LABEL
    | alphaAttr
    ;

alphaAttr
    : ALPHA INDEX
    ;

emptyBinding
    : attribute ARROW EMPTY
    ;

deltaBidning
    : DELTA DASHED_ARROW BYTES
    ;

lambdaBidning
    : LAMBDA DASHED_ARROW FUNCTION
    ;

FUNCTION
    : [A-Z] ~[ \r\n\t,.|':;!\-?\][}{)(⟧⟦]*
    ;

application
    : (formation | dispatch | termination) bnds
    ;

bnds: (LB bindings RB)+
    ;

dispatch
    : (formation | termination) bnds? attr+ disp
    | HOME attr+ disp
    | XI attr+ disp
    ;

disp:
    | dispBnds attr+ disp
    ;

// The rule was separately because it's used as
// marker where it's needed to enter the <o> object
// in order to make application right
dispBnds
    : bnds
    ;

attr: DOT attribute
    ;

termination
    : ERROR
    ;

LCB : '{'
    ;
RCB : '}'
    ;
LSB : '⟦'
    ;
RSB : '⟧'
    ;
LB  : '('
    ;
RB  : ')'
    ;
DOT : '.'
    ;
COMMA
    : ','
    ;
ARROW
    : '↦'
    ;
DASHED_ARROW
    : '⤍'
    ;
ALPHA
    : 'α'
    ;
EMPTY
    : '∅'
    ;
PHI : 'φ'
    ;
RHO : 'ρ'
    ;
SIGMA
    : 'σ'
    ;
VTX : 'ν'
    ;
DELTA
    : 'Δ'
    ;
XI  : 'ξ'
    ;
LAMBDA
    : 'λ'
    ;
HOME: 'Φ'
    ;
ERROR
    : '⊥'
    ;
MINUS
    : '-'
    ;

INDEX
    : [0-9]
    | [1-9][0-9]*
    ;

LABEL
    : [a-z] ~[ \r\n\t,.|':;!?\][}{)(⟧⟦]*
    ;

fragment BYTE
    : [0-9A-F][0-9A-F]
    ;

BYTES
    : MINUS MINUS
    | BYTE MINUS
    | BYTE (MINUS BYTE)+
    ;