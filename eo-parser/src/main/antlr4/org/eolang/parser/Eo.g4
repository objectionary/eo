/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
grammar Eo;

tokens { TAB, UNTAB }

// Entry point
program
    : metas? object EOF
    ;

// Double EOL
eop : EOL EOL
    ;

// Metas
metas
    : (META EOL)* META eop
    ;

comment
    : COMMENTARY EOL
    ;

commentOptional
    : comment*
    ;

// Object
// Ends on the next line
object
    : commentOptional masterBody
    | bound
    ;

// Objects that may be used inside abstract object
// Ends on the next line
bound
    : commentOptional (application | ((method | just) onameOrTname) EOL)
    ;

tbound
    : commentOptional (tapplication | ((method | just) tname) EOL)
    ;

tsubMaster
    : commentOptional tmasterBody
    ;

subMaster
    : commentOptional masterBody
    ;

masterBody
    : formation
    | (atom | hanonym onameOrTname EOL)
    ;

tmasterBody
    : tformation
    | (atom | hanonym (tname) EOL)
    ;

// Just an object reference without name
just: beginner
    | finisher
    ;

// Atom - abstract object with mandatory name
// Can't contain inner objects
atom: voids suffix SPACE QUESTION testsOrEol
    ;

// Formation - abstract object with mandatory name
// Can contain inner objects
// Ends on the next line
formation
    : voids onameOrTname innersOrEol
    ;

tformation
    : voids tname innersOrEol
    ;

testsOrEol
    : tests
    | EOL
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
    : EOL TAB (bound | subMaster) (bound | EOL? subMaster)* UNTAB
    ;

// Tests
tests
    : EOL TAB (tbound | tsubMaster) (tbound | EOL? tsubMaster)* UNTAB
    ;

// Void attributes of an abstract object, atom or horizontal anonym object
voids
    : LSQ (void (SPACE void)*)? RSQ
    ;

// Void attribute
void: NAME
    | PHI
    ;

// Application
// - horizontal
// - vertical
// Ends on the next line
application
    : happlicationExtended onameOrTname EOL
    | vapplication
    ;

tapplication
    : happlicationExtended tname EOL
    | tvapplication
    ;

// Horizontal application
// The whole application is written in one line
// The head does not contain elements in vertical notation
happlication
    : happlicationHead happlicationTail
    | happlicationReversed
    ;

happlicationReversedHead
    : reversed SPACE happlicationReversedFirst
    ;

// Extended horizontal application
// The head can contain elements in horizontal or vertical notations
happlicationExtended
    : happlicationHeadExtended happlicationTail
    | happlicationReversed
    ;

// Reversed horizontal application
happlicationReversed
    : happlicationReversedHead happlicationTail?
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

// Horizontal application argument without binding
happlicationArgUnbound
    : SPACE happlicationArg
    ;

// Tail for horizontal application with scoped object as the last argument
happlicationTailScoped
    : happlicationArgUnbound* SPACE happlicationArgScoped
    ;

// Horizontal application tail
happlicationTail
    : (happlicationArgUnbound as)+
    | happlicationArgUnbound+
    ;

// The rule is separated because we should enter to the last object
// here, but don't do it on happlicationTail rule
happlicationReversedFirst
    : happlicationArg
    ;

// Argument of horizontal application
// Does not contain elements in vertical notation
happlicationArg
    : just
    | hmethod
    | scope
    ;

happlicationArgScoped
    : voids aname innersOrEol
    ;

// Vertical application
// Ends on the next line
vapplication
    : vapplicationHead onameOrTname vapplicationArgs
    | reversed onameOrTname vapplicationArgsReversed
    ;

tvapplication
    : vapplicationHead tname vapplicationArgs
    | reversed tname vapplicationArgsReversed
    ;

// Vertical application head
vapplicationHead
    : applicable
    | hmethod
    | vmethod
    | compactTuple
    ;

// Compact tuple
compactTuple
    : (hmethod | applicable | reversed) SPACE STAR INT?
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
// Ends on the next line
vapplicationArgsSpecific
    : vapplicationArgBound+
    | vapplicationArgUnbound+
    ;

// Vertical application arguments with bindings
vapplicationArgBound
    : vapplicationArgBoundCurrent EOL
    | vapplicationArgBoundNext
    ;

// Vertical application arguments with bindings
// Ends on the current line
vapplicationArgBoundCurrent
    : LB happlicationExtended RB as oname? // horizontal application
    | commentOptional LB hanonym RB as fname? // horizontal anonym object
    | (just | method) as oname? // just an object reference | method
    ;

// Vertical application arguments with bindings
// Ends on the next line
vapplicationArgBoundNext
    : commentOptional voids as fname? innersOrEol // vertical anonym object
    | vapplicationHead as oname? vapplicationArgs // vertical application
    | reversed as oname? vapplicationArgsReversed // reversed vertical application
    ;

// Vertical application arguments without bindings
// Ends on the next line
vapplicationArgUnbound
    : vapplicationArgUnboundCurrent EOL
    | vapplicationArgUnboundNext
    ;

// Vertical application arguments without bindings
// Ends on the current line
vapplicationArgUnboundCurrent
    : happlicationExtended oname? // horizontal application
    | commentOptional hanonym fname? // horizontal anonym object
    | (just | method) oname? // just an object reference or method
    ;

// Vertical application arguments without bindings
// Ends on the next line
vapplicationArgUnboundNext
    : formationNamed // vertical abstract object
    | vapplicationHead oname? vapplicationArgs // vertical application
    | reversed oname? vapplicationArgsReversed // reversed vertical application
    | (happlicationHead | happlicationReversedHead) happlicationTailScoped // scoped horizontal application
    ;

formationNamed
    : commentOptional voids fname? innersOrEol
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
    : arrow voids
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

// Horizontal method
// The whole method is written in one line
// The head does not contain elements in vertical notation
hmethod
    : (just | scope) methodTail+
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
    : vmethodHead methodTail vmethodHeadApplicationTail
    | vmethodHeadVapplication
    | ((just | hanonym) oname?) EOL
    | formationNamed
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

// Auto object name
aname
    : ARROW ARROW CONST?
    ;

// Formation name
fname
    : oname
    | SPACE aname
    ;

// Either object name or test name
onameOrTname
    : oname | tname
    ;

// Object name
oname
    : suffix CONST?
    ;

tname
    : tarrow (PHI | NAME)
    ;

tarrow
    : SPACE PLUS ARROW SPACE
    ;

// Suffix
suffix
    : arrow (PHI | NAME) APOSTROPHE?
    ;

arrow
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
TILDE
    : '~'
    ;
APOSTROPHE
    : '\''
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

NAME: [a-z] ~[ \r\n\t,.|':;!?\][}{)(🌵]*
    ;

fragment TEXT_MARK
    : '"""'
    ;

TEXT: TEXT_MARK ('\n' | '\r\n') (~[\\] | ESCAPE_SEQUENCE)*? TEXT_MARK
    ;
