/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
grammar Eo;

tokens { TAB, UNTAB }

// Entry point
program
    : license? metas? objects EOF
    ;

// Double EOL
eop : EOL EOL
    ;

// Licence
license
    : (COMMENTARY EOL)* COMMENTARY eop
    ;

// Metas
metas
    : (META EOL)* META eop
    ;

// Objects
// Ends on the next line
objects
    : (object EOL?)* object
    ;

comment
    : COMMENTARY EOL
    ;

commentOptional
    : comment*
    ;

commentMandatory
    : comment+
    ;

// Object
// Ends on the next line
object
    : master
    | slave
    ;

// Objects that may be used inside abstract object
// Ends on the next line
slave
    : commentOptional (application | (methodNamed | justNamed) EOL)
    ;

// Indeprendent objects that may have slaves (except atom)
// Ends on the next line
master
    : commentMandatory masterBody
    ;

subMaster
    : commentOptional masterBody
    ;

masterBody
    : formation
    | (atom | hanonym oname) EOL
    ;

// Just an object reference without name
just: beginner
    | finisher
    ;

// Just object reference with optional name
justNamed
    : just oname?
    ;

// Atom - abstract object with mandatory name and type
// Can't contain inner objects
atom: voids suffix type
    ;

// Formation - abstract object with mandatory name
// Can contain inner objects
// Ends on the next line
formation
    : voids oname innersOrEol
    ;

// Inners object inside formation or EOL
innersOrEol
    : inners
    | EOL
    ;

// Inner objects inside abstraction
// Every inner object must be indented
// Ends on the next line
// No empty lines before "slave"
// May be one empty line before "master"
inners
    : EOL TAB (slave | subMaster) (slave | EOL? subMaster)* UNTAB
    ;

// Void attributes of an abstract object, atom or horizontal anonym object
voids
    : LSQ (void (SPACE void)*)? RSQ
    ;

// Void attribute
void: NAME
    | PHI
    ;

// Type of atom
type: SPACE SLASH typeFqn
    ;

// Type FQN
typeFqn
    : NAME (DOT NAME)*
    ;

// Application
// - horizontal
// - vertical
// Ends on the next line
application
    : happlicationExtended oname? EOL
    | vapplication
    ;

// Horizontal application
// The whole application is written in one line
// The head does not contain elements in vertical notation
happlication
    : happlicationHead happlicationTail
    | happlicationReversed
    ;

// Extended horizontal application
// The head can contain elements in horizontal or vertical notations
happlicationExtended
    : happlicationHeadExtended happlicationTail
    | happlicationReversed
    ;

// Reversed horizontal application
happlicationReversed
    : reversed happlicationTailReversed
    ;

// Head of horizontal application
// Does not contain elements in vertical notation
happlicationHead
    : hmethod
    | applicable
    ;

// Extended head of horizontal application
// Can contain elements in vertical notation
happlicationHeadExtended
    : vmethod
    | happlicationHead
    ;

// Simple statements that can be used as head of application
applicable
    : STAR
    | NAME
    | PHI
    ;

// Horizontal application tail
happlicationTail
    : (SPACE happlicationArg as)+
    | (SPACE happlicationArg)+
    ;

happlicationTailReversed
    : SPACE happlicationTailReversedFirst happlicationTail?
    ;

// The rule is separated because we should enter to the last object
// here, but don't do it on happlicationTail rule
happlicationTailReversedFirst
    : happlicationArg
    ;

// Argument of horizontal application
// Does not contain elements in vertical notation
happlicationArg
    : just
    | hmethod
    | scope
    ;

// Vertical application
// Ends on the next line
vapplication
    : vapplicationHeadNamed vapplicationArgs
    | reversed oname? vapplicationArgsReversed
    ;

// Vertical application head
vapplicationHead
    : applicable
    | hmethod
    | vmethod
    | compactArray
    ;

// Compact arrays
compactArray
    : NAME SPACE STAR INT?
    ;

// Vertical application head with optional name
vapplicationHeadNamed
    : vapplicationHead oname?
    ;

// Vertical application head with binding
vapplicationHeadAs
    : vapplicationHead as
    ;

// Vertical application arguments
// Ends on the next line
vapplicationArgs
    : EOL TAB vapplicationArgsSpecific UNTAB
    ;

// Arguments for reversed vertical application
vapplicationArgsReversed
    : EOL TAB vapplicationArgUnbound vapplicationArgsSpecific? UNTAB
    ;

// Arguments of vertical application
// Must either all bound or all unbound
// Comments between vertical arguments are prohibited
// Ends on the next line
vapplicationArgsSpecific
    : vapplicationArgBound+
    | vapplicationArgUnbound+
    ;

// Prohibited comment between vertical arguments
// If this rule is matched, the parser will throw an error
prohibitedComment
    : comment
    ;

// Vertical application arguments with bindings
vapplicationArgBound
    : vapplicationArgBoundCurrent EOL
    | vapplicationArgBoundNext
    | prohibitedComment+
    ;

// Vertical application arguments with bindings
// Ends on the current line
vapplicationArgBoundCurrent
    : vapplicationArgHapplicationBound // horizontal application
    | vapplicationArgHanonymBound // horizontal anonym object
    | (just | method) as oname? // just an object reference | method
    ;

// Vertical application arguments with bindings
// Ends on the next line
vapplicationArgBoundNext
    : vapplicationArgVanonymBound // vertical anonym object
    | vapplicationHeadAs oname? vapplicationArgs // vertical application
    | reversed as oname? vapplicationArgsReversed // reversed vertical application
    ;

// Vertical application arguments without bindings
// Ends on the next line
vapplicationArgUnbound
    : vapplicationArgUnboundCurrent EOL
    | vapplicationArgUnboundNext
    | prohibitedComment+
    ;

// Vertical application arguments without bindings
// Ends on the current line
vapplicationArgUnboundCurrent
    : vapplicationArgHapplicationUnbound // horizontal application
    | vapplicationArgHanonymUnbound // horizontal anonym object
    | justNamed // just an object reference
    | methodNamed // method
    ;

// Vertical application arguments without bindings
// Ends on the next line
vapplicationArgUnboundNext
    : formationNamedOrNameless // vertical abstract object
    | vapplicationHeadNamed vapplicationArgs // vertical application
    | reversed oname? vapplicationArgsReversed // reversed vertical application
    ;

// Horizontal application as argument of vertical application
vapplicationArgHapplicationBound
    : LB happlicationExtended RB as oname?
    ;

vapplicationArgHapplicationUnbound
    : happlicationExtended oname?
    ;

formationNameless
    : voids aname? innersOrEol
    ;

// Formation with or without name
formationNamedOrNameless
    : commentOptional formation
    | formationNameless
    ;

// Bound vertical anonym abstract object as argument of vertical application argument
// Ends on the next line
vapplicationArgVanonymBound
    : commentOptional attributesAs oname innersOrEol
    | attributesAs aname? innersOrEol
    ;

attributesAs
    : voids as
    ;

vapplicationArgHanonymBoundBody
    : LB hanonym RB as
    ;

// Horizontal anonym abstract object as argument of vertical application
vapplicationArgHanonymBound
    : commentOptional vapplicationArgHanonymBoundBody oname
    | vapplicationArgHanonymBoundBody aname?
    ;

vapplicationArgHanonymUnbound
    : commentOptional hanonym oname
    | hanonym aname?
    ;

// Horizontal formation
hformation
    : voids hanonymInner+
    ;

// Horizontal anonym object
hanonym
    : hformation
    | onlyphi
    ;

// Unnamed abstract object with only @-bound attribute
// x.y.z > [i]          -> [i] (x.y.z > @)
// x y z > [i]          -> [i] (x y z > @)
// [a] (b > c) > [i]    -> [i] ([a] (b > c) > @)
// a > [i] > [j]        -> [j] ([i] (a > @) > @)
// x > [i]              -> [i] (x > @)
onlyphi
    : (hmethod | happlication | hformation | just) onlyphiTail
    | onlyphi onlyphiTail
    ;

// Tail of the unnamed abstract object with only @-bound attribute
onlyphiTail
    : spacedArrow voids
    ;

// Inner object of horizontal anonym object
// Does not contain elements in vertical notation
hanonymInner
    : SPACE LB (hmethod | happlication | hanonym | just) oname RB
    ;

// Method
method
    : hmethod
    | vmethod
    ;

// Method with optional name
methodNamed
    : method oname?
    ;

// Horizontal method
// The whole method is written in one line
// The head does not contain elements in vertical notation
hmethod
    : hmethodHead methodTail+
    ;

// Head of horizontal method
hmethodHead
    : just
    | scope
    ;

// Vertical method
vmethod
    : vmethodHead methodTail
    ;

// Head of vertical method
// The simple variation of this block leads to left recursion error
// So in order to avoid it this block was described in more detail
// Head of vertical method can be:
// 1. vertical method
// 2. vertical application
// 3. just an object reference
// 4. vertical formation
// 5. unnamed abstract object with only @-bound attribute
// Ends on the next line
vmethodHead
    : vmethodHead methodTailOptional vmethodHeadApplicationTail
    | vmethodHeadVapplication
    | (justNamed | hanonym oname?) EOL
    | formationNamedOrNameless
    ;

methodTailOptional
    : methodTail
    ;

vmethodHeadApplicationTail
    : oname? (vapplicationArgs | EOL)
    | happlicationTail oname? EOL
    ;

// Vertical application as head of vertical method
// Ends on the next line
vmethodHeadVapplication
    : (applicable | hmethod) oname? vapplicationArgs
    | reversed oname? vapplicationArgsReversed
    ;

// Tail of method
methodTail
    : DOT finisher
    ;

// Can be at the beginning of the statement
// Can't be after DOT
beginner
    : STAR
    | ROOT
    | HOME
    | XI
    | data
    ;

// Can start or finish the statement
finisher
    : NAME
    | PHI
    | RHO
    ;

// Reversed notation
// Only finisher can be used in reversed notation
reversed
    : finisher DOT
    ;

// Object name
oname
    : suffix CONST?
    ;

// Automatic name of the object
aname
    : SPACE ARROW ARROW
    ;

// Suffix
suffix
    : spacedArrow (PHI | NAME)
    ;

spacedArrow
    : SPACE ARROW SPACE
    ;

// Simple scope
// Does not contain elements in vertical notation
// Is used in happlicationArg, hmethodHead
scope
    : LB (happlication | hanonym) RB
    ;

// Binding
as  : COLON (NAME | INT)
    ;

// Data
data: BYTES
    | TEXT
    | STRING
    | INT
    | FLOAT
    | HEX
    ;

COMMENTARY
    : HASH
    | (HASH ~[\r\n]* ~[\r\n\t ])
    ;
META: PLUS NAME (SPACE ~[\t\r\n ]+)*
    ;

ROOT: 'Q'
    ;
HOME: 'QQ'
    ;
STAR: '*'
    ;
CONST
    : '!'
    ;
SLASH
    : '/'
    ;
COLON
    : ':'
    ;
ARROW
    : '>'
    ;
XI  : '$'
    ;
PLUS: '+'
    ;
MINUS
    : '-'
    ;
QUESTION
    : '?'
    ;
SPACE
    : ' '
    ;
DOT : '.'
    ;
LSQ : '['
    ;
RSQ : ']'
    ;
LB  : '('
    ;
RB  : ')'
    ;
PHI : '@'
    ;
RHO : '^'
    ;
HASH: '#'
    ;
BAR : '|'
    ;

fragment INDENT
    : SPACE SPACE
    ;

fragment LINEBREAK
    : '\n'
    | '\r\n'
    ;

EOL : LINEBREAK INDENT*
    ;

fragment BYTE
    : [0-9A-F][0-9A-F]
    ;

fragment EMPTY_BYTES
    : MINUS MINUS
    ;
fragment LINE_BYTES
    : BYTE (MINUS BYTE)+
    ;

BYTES
    : EMPTY_BYTES
    | BYTE MINUS
    | LINE_BYTES (MINUS EOL LINE_BYTES)*
    ;

fragment ESCAPE_SEQUENCE
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ BYTE BYTE
    ;

STRING
    : '"' (~["\\\r\n] | ESCAPE_SEQUENCE)* '"'
    ;

fragment ZERO
    : '0'
    ;

INT : (PLUS | MINUS)? (ZERO | ZERO?[1-9][0-9]*)
    ;

fragment EXPONENT
    : ('e'|'E') (PLUS | MINUS)? ('0'..'9')+
    ;

FLOAT
    : (PLUS | MINUS)? [0-9]+ DOT [0-9]+ EXPONENT?
    ;

HEX : '0x' [0-9a-fA-F]+
    ;

NAME: [a-z] ~[ \r\n\t,.|':;!?\][}{)(]*
    ;

fragment TEXT_MARK
    : '"""'
    ;

TEXT: TEXT_MARK ('\n' | '\r\n') (~[\\] | ESCAPE_SEQUENCE)*? TEXT_MARK
    ;
