grammar Program;

tokens { TAB, UNTAB }

// Entry point
program
    : license? metas? objects EOF
    ;

// Licence
license
    : (COMMENT EOL)* COMMENT EOP
    ;

// Metas
metas
    : (META EOL)* META EOP
    ;

// Objects
objects
    : ((COMMENT EOL)* object (EOL | EOP))+
    ;

// Object. Can be:
// - atom
// - abstraction with mandatory name
// - horizontal anonym object with mandatory name
// - application (vertical or horizontal) with optional name
// - method (vertical or horizontal) with optional name
// - just (just object reference) with optional name
object
    : atom
    | abstraction
    | hanonym oname
    | application
    | methodNamed
    | justNamed
    ;

// Just object reference without name
just: beginner
    | finisherOrCopy
    | versioned
    ;

// Just object reference with optional name
// Can't be used with "spread" operator (DOTS)
justNamed
    : just oname?
    ;

// Just object reference with binding
// Used only as argument in vertical application
// Can be used with "spread" operator (DOTS)
justHas
    : just has
    ;

// Just object reference with binding with optional name
// Used only as argument in vertical application
// Can't be used with "spread" operator (DOTS)
justHasNamed
    : justHas oname?
    ;

// Atom - abstract object with mandatory name and type
// Comment can be placed before atom
// Can't contain inner objects
atom: ahead suffix type
    ;

// Abstraction - abstract object with mandatory name
// Comment can be placed before atom
// Can contain any inner object
abstraction
    : ahead oname inners?
    ;

// Inner objects inside abstraction
// Every inner object must have next indentation level
inners
    : EOL TAB (object (EOL | EOP))+ UNTAB
    ;

// Attributes of an abstract object, atom or horizontal anonym object
// Separated by "SPACE"
// If "vararg" object (obj...) is present - must be the last one in the list
attributes
    : LSQ
      ((attribute (SPACE attribute)* (SPACE vararg)?)? | vararg)
      RSQ
    ;

// Attribute
attribute
    : NAME
    ;

// Type of atom
type: SPACE SLASH (NAME | QUESTION)
    ;

// Vararg attribute
vararg
    : NAME DOTS
    ;

// Application
// - horizontal with optional name
// - vertical
application
    : happlicationNamed
    | vapplication
    ;

// Horizontal application with optional name
happlicationNamed
    : happlicationExtended oname?
    ;

// Horizontal application
// The whole application is written in one line
// The head can't be anything in vertical notation
happlication
    : happlicationHead happlicationTail
    ;

// Extended horizontal application
// The head of the application can be anything in horizontal or vertical notations
happlicationExtended
    : happlicationHeadExtended happlicationTailExtended
    ;

happlicationHead
    : hmethod
    | applicable
    | scope
    ;

happlicationHeadExtended
    : vmethod
    | hmethodExtended
    | applicable
    | scopeExtended
    ;

applicable
    : STAR
    | (NAME | AT) COPY?
    | reversed
    ;

happlicationTail
    : (SPACE (happlicationArg | happlicationArgHas))+
    ;

happlicationArg
    : beginner
    | finisherOrCopy
    | DOTS? (spreadable | hmethod | scope)
    ;

happlicationArgHas
    : happlicationArg has
    ;

happlicationTailExtended
    : (SPACE (happlicationArgExtended | happlicationArgExtendedHas))+
    ;

happlicationArgExtended
    : beginner
    | finisherOrCopy
    | DOTS? (spreadable | hmethodExtended | scopeExtended)
    | LB DOTS spreadable RB
    ;

happlicationArgExtendedHas
    : happlicationArgExtended has
    ;

vapplication
    : vapplicationHeadNamed vapplicationArgs
    ;

vapplicationHead
    : applicable
    | hmethodExtended
    | hmethodExtendedVersioned
    | vmethod
    | vmethodVersioned
    | versioned
    ;

vapplicationHeadNamed
    : vapplicationHead oname?
    ;

vapplicationArgs
    : EOL
      TAB
      (
        ( vapplicationArgVanonym
        | vapplicationArgHanonym
        | vapplicationArgHapplicationNamed
        | vapplicationArgVapplicationNamed
        | justNamed
        | justHasNamed
        | methodNamed
        | methodHasNamed
        | vapplicationArgSpreadable
        )
        (EOL | EOP)
      )+
      UNTAB
    ;

vapplicationArgSpreadable
    : DOTS?
      ( just
      | justHas
      | method
      | methodHas
      | vapplicationArgHapplication
      | vapplicationArgVapplication
      )
    ;

vapplicationArgHapplication
    : happlicationExtended
    | LB happlicationExtended RB has
    ;

vapplicationArgHapplicationNamed
    : vapplicationArgHapplication oname?
    ;

vapplicationArgVapplication
    : (vapplicationHead | vapplicationHeadHas) vapplicationArgs
    ;

vapplicationArgVapplicationNamed
    : (vapplicationHeadNamed | vapplicationHeadHasNamed) vapplicationArgs
    ;

vapplicationHeadHas
    : (applicable | hmethodExtended | hmethodExtendedVersioned | versioned) has
    ;

vapplicationHeadHasNamed
    : vapplicationHeadHas oname?
    ;

vapplicationArgVanonym
    : attributes has? oname? abstractees?
    ;

vapplicationArgHanonym
    : (hanonym | LB hanonym RB has) oname?
    ;

hanonym
    : attributes hanonymInner+
    ;

hanonymInner
    : SPACE LB (hmethod | hmethodVersioned | happlication | hanonym | just) oname RB
    ;

abstractees
    : EOL
      TAB
      ((innerabstract | application | justNamed | methodNamed) (EOL | EOP))+
      UNTAB
    ;

innerabstract
    : ahead oname? abstractees?
    ;

ahead
    : (COMMENT EOL)* attributes
    ;

method
    : hmethodExtended
    | hmethodExtendedVersioned
    | vmethod
    | vmethodVersioned
    ;

methodNamed
    : method oname?
    ;

methodHas
    : (hmethodExtended | hmethodExtendedVersioned) has
    ;

methodHasNamed
    : methodHas oname?
    ;

// Horizontal method
// The whole method is written in one line
// The head can't be anything in vertical notation
hmethod
    : hmethodHead methodTail+
    ;

// Extended horizontal method
// The head of the method can be anything in horizontal or vertical notations
hmethodExtended
    : hmethodHeadExtended methodTail+
    ;

// Versioned horizontal method
// The whole method is written in one line
// The head can't be anything in vertical notation
hmethodVersioned
    : hmethodHead methodTail* methodTailVersioned
    ;

// Versioned extended horizontal method
// The head of the method can be anything in horizontal or vertical notations
hmethodExtendedVersioned
    : hmethodHeadExtended methodTail* methodTailVersioned
    ;

hmethodHead
    : beginner
    | finisherOrCopy
    | scope
    ;

hmethodHeadExtended
    : beginner
    | finisherOrCopy
    | scopeExtended
    ;

vmethod
    : vmethodHead vmethodTail
    ;

vmethodVersioned
    : vmethodHead vmethodTailVersioned
    ;

// Head of vertical method
// The simple variation of this block leads to left recursion error
// So in order to avoid it this block was described in more detail
// Head of vertical method can be:
// 1. vertical method
// 2. horizontal method
// 3. vertical application. Here, vertical application is split into 2 parts because
//    vapplicationHead contains vmethod which leads to left recursion error.
// 4. horizontal application. The same logic as with a vertical application
// 5. just an object reference
vmethodHead
    : vmethodHead (vmethodTail | vmethodTailVersioned) oname?                                               // vmethod
    | (hmethodExtended | hmethodExtendedVersioned) oname?                                                   // hmethod extended
    | vmethodHead (vmethodTail | vmethodTailVersioned) oname? vapplicationArgs oname?                       // vmethod + vapplication
    | (applicable | hmethodExtended | hmethodExtendedVersioned | versioned) oname? vapplicationArgs oname?  // vapplication without vmethod in head
    | vmethodHead (vmethodTail | vmethodTailVersioned) happlicationTailExtended oname?                      // vmethod + haplication
    | (applicable | hmethodExtended) happlicationTailExtended oname?                                        // happlication without vmethod in head
    | justNamed                                                                                             // just
    ;

// Tail of vertical method
vmethodTail
    : EOL methodTail
    ;

// Versioned tail of vertical method
vmethodTailVersioned
    : EOL methodTailVersioned
    ;

// Tail of method
methodTail
    : DOT mtd
    ;

// Versioned tail of method
methodTailVersioned
    : DOT mtdVersioned
    ;

// Can be at the beginning of the statement
// Can't be in the middle or at the end
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
    | AT
    | RHO
    | SIGMA
    | VERTEX
    ;

// Something that can be spread
spreadable
    : (NAME | AT | RHO | SIGMA) COPY?
    ;

// Method after DOT
mtd : finisherOrCopy
    ;

// Method with version after DOT
mtdVersioned
    : NAME version?
    ;

finisherOrCopy
    : finisher COPY?
    ;

versioned
    : NAME version?
    ;

reversed
    : finisher DOT
    ;

oname
    : suffix CONST?
    ;

suffix
    : SPACE ARROW SPACE label
    ;

label
    : AT
    | NAME
    ;

scope
    : LB (happlication | hmethod | hanonym) RB
    ;

scopeExtended
    : LB (happlicationExtended | hmethodExtended | hanonym) RB
    ;

version
    : BAR VER
    ;

has : COLON (NAME | RHO)
    ;

data: BYTES
    | BOOL
    | TEXT
    | STRING
    | INT
    | FLOAT
    | HEX
    ;

COMMENT
    : HASH
    | (HASH ~[\r\n]* ~[\r\n\t ])
    ;
META: PLUS NAME (SPACE ~[\r\n]+)?
    ;

ROOT: 'Q'
    ;
HOME: 'QQ'
    ;
STAR: '*'
    ;
DOTS: '...'
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
COPY: '\''
    ;
ARROW
    : '>'
    ;
VERTEX
    : '<'
    ;
SIGMA
    : '&'
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
AT  : '@'
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

EOP : LINEBREAK LINEBREAK INDENT*
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

BOOL: 'TRUE'
    | 'FALSE'
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
    :
    (PLUS | MINUS)? [0-9]+ DOT [0-9]+ EXPONENT?
    ;

HEX : '0x' [0-9a-fA-F]+
    ;

NAME: [a-z] ~[ \r\n\t,.|':;!?\][}{)(]*
    ;

VER : [0-9]+ DOT [0-9]+ DOT [0-9]+
    ;

fragment TEXT_MARK
    : '"""'
    ;

TEXT: TEXT_MARK ('\n' | '\r\n') (~[\\] | ESCAPE_SEQUENCE)*? TEXT_MARK
    ;
