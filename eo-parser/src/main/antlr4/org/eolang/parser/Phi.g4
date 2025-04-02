/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
grammar Phi;

// Skip spaces, tabs, newLines
WS  : [ \t\r\n]+ -> skip
    ;

program
    : HOME ARROW object
    | LCB object RCB
    ;

object
    : formation (dispatch applicationsOrDispatches)?
    | (scoped | termination | data) applicationsOrDispatches
    ;

formation
    : LSB bindings RSB
    ;

scoped
    : XI
    | HOME
    | DEF_PACKAGE
    | fullAttribute
    ;

bindings
    : binding?
    | binding (COMMA binding)+
    ;

binding
    : tauBinding
    | emptyBinding
    | deltaBinding
    | lambdaBinding
    ;

tauBinding
    : attribute ARROW object
    | attribute voids ARROW formation
    ;

applicationTauBinding
    : fullAttribute ARROW object
    | fullAttribute voids ARROW formation
    ;

voids
    : LB (void? | void (COMMA void)+) RB
    ;

void: LABEL
    | ALPHA
    | PHI
    ;

attribute
    : PHI
    | RHO
    | LABEL
    ;

fullAttribute
    : attribute
    | ALPHA
    ;

emptyBinding
    : attribute ARROW EMPTY
    ;

deltaBinding
    : DELTA DASHED_ARROW (BYTES | EMPTY)
    ;

lambdaBinding
    : LAMBDA DASHED_ARROW FUNCTION
    ;

FUNCTION
    : [A-Z][A-Z0-9a-z_φ]*
    ;

application
    : LB (applicationBindings | applicationObjects) RB
    ;

applicationBindings
    : applicationTauBinding?
    | applicationTauBinding (COMMA applicationTauBinding)*
    ;

applicationObjects
    : justObject?
    | justObject (COMMA justObject)+
    ;

justObject
    : object
    ;

dispatch: DOT fullAttribute
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
EMPTY
    : '∅'
    ;
PHI : 'φ'
    ;
RHO : 'ρ'
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
DEF_PACKAGE
    : 'Φ̇'
    ;
ERROR
    : '⊥'
    ;
MINUS
    : '-'
    ;

data: STRING
    | INT
    | FLOAT
    ;

INT : (PLUS | MINUS)? ('0' | '0'?[1-9][0-9]*)
    ;

FLOAT
    : (PLUS | MINUS)? [0-9]+ DOT [0-9]+ EXPONENT?
    ;

STRING
    : '"' (~["\\\r\n] | ESCAPE_SEQUENCE)* '"'
    ;

fragment ESCAPE_SEQUENCE
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ BYTE BYTE
    ;

fragment EXPONENT
    : ('e'|'E') (PLUS | MINUS)? ('0'..'9')+
    ;

PLUS: '+'
    ;

LABEL
    : [a-z] ~[ \r\n\t,.|':;!?\][}{)(⟧⟦]*
    ;

ALPHA
    : 'α' ([0-9] | [1-9][0-9]*)
    ;

fragment BYTE
    : [0-9A-F][0-9A-F]
    ;

BYTES
    : MINUS MINUS
    | BYTE MINUS
    | BYTE (MINUS BYTE)+
    ;
