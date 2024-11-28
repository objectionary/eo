/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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
    | (scoped | termination) applicationsOrDispatches
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
    ;

attribute
    : PHI
    | RHO
    | LABEL
    | alphaAttr
    ;

alphaAttr
    : ALPHA INDEX
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
    : [A-Z] ~[ \r\n\t,.|':;!\-?\][}{)(⟧⟦]*
    ;

application
    : LB applicationBinding RB
    ;

applicationBinding
    : tauBinding?
    | tauBinding (COMMA tauBinding)*
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
