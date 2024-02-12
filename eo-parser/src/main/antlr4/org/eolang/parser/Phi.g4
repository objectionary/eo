grammar Phi;

// Skip spaces, tabs, newLines
WS  : [ \t\r\n]+ -> skip
    ;

program
    : HOME ARROW object
    | LCB object RCB
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
    : tauBinding
    | emptyBinding
    | deltaBidning
    | lambdaBidning
    ;

tauBinding
    : attribute ARROW object
    ;

attribute
    : PHI
    | RHO
    | SIGMA
    | VTX
    | LABEL
    | tauAttr
    ;

tauAttr
    : TAU INDEX
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
    | (HOME | XI) (attr+ disp)?
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
TAU
    : 'τ'
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