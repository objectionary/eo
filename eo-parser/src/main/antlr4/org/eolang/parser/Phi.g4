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
    | (scoped | termination | data) applicationsOrDispatches
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
    : [A-Z] ~[ \r\n\t,.|':;!\-?\][}{)(⟧⟦]*
    ;

application
    : LB (applicationBindings | applicationObjects) RB
    ;

applicationBindings
    : tauBinding?
    | tauBinding (COMMA tauBinding)*
    ;

applicationObjects
    : justObject?
    | justObject (COMMA justObject)+
    ;

justObject
    : object
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
