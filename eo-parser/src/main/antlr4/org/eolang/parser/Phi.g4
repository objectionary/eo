grammar Phi;

// Skip spaces, tabs, newLines
WS  : [ \t\r\n]+ -> skip
    ;

program
    : HOME ARROW object
    | LCB object RCB
    ;

object
    : formation applicationsOrDispatches
    | scoped (dispatch applicationsOrDispatches)?
    | termination
    ;

formation
    : LSB bindings RSB
    ;

scoped
    : XI
    | HOME
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
    : DELTA DASHED_ARROW (BYTES | EMPTY)
    ;

lambdaBidning
    : LAMBDA DASHED_ARROW FUNCTION
    ;

FUNCTION
    : [A-Z] ~[ \r\n\t,.|':;!\-?\][}{)(⟧⟦]*
    ;

application
    : LB bindings RB
    ;

dispatch: DOT attribute
    ;

applicationsOrDispatches
    : (application | dispatch)*
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
