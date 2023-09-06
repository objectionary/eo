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

// Object
object
    : atom          // Atom
    | abstraction   // Abstraction
    | hanonym oname // Horizontal anonym with name
    | application   // Application
    | methodNamed   // Method
    | justNamed     // Just an object reference
    ;

// Just an object reference without name
just: beginner
    | finisherCopied
    | versioned
    ;

// Just object reference with optional name
// Can't be used with "spread" operator (DOTS)
justNamed
    : just oname?
    ;

// Atom - abstract object with mandatory name and type
// Comment can be placed before atom
// Can't contain inner objects
atom: ahead suffix type
    ;

// Abstraction - abstract object with mandatory name
// Comment can be placed before atom
// Can contain inner objects
abstraction
    : ahead oname inners?
    ;

// Inner objects inside abstraction
// Every inner object must be indented
inners
    : EOL TAB (object (EOL | EOP))+ UNTAB
    ;

// Attributes of an abstract object, atom or horizontal anonym object
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
    : happlicationExtended oname?
    | vapplication
    ;

// Horizontal application
// The whole application is written in one line
// The head does not contain elements in vertical notation
// The division of elements into regular and extended ones is
// due to the presence of horizontal anonymous objects where inner objects
// must be horizontal only
happlication
    : happlicationHead happlicationTail
    ;

// Extended horizontal application
// The head can contain elements in horizontal or vertical notations
happlicationExtended
    : happlicationHeadExtended happlicationTailExtended
    ;

// Head of horizontal application
// Does not contain elements in vertical notation
happlicationHead
    : hmethod
    | applicable
    | scope
    ;

// Extended head of horizontal application
// Can contain elements in vertical notation
happlicationHeadExtended
    : vmethod
    | hmethodExtended
    | applicable
    | scopeExtended
    ;

// Simple statements that can be used as head of application
applicable
    : STAR
    | (NAME | AT) COPY?
    | reversed
    ;

// Horizontal application tail
happlicationTail
    : (SPACE (happlicationArg | happlicationArg as))+
    ;

// Argument of horizontal application
// Does not contain elements in vertical notation
happlicationArg
    : beginner
    | finisherCopied
    | DOTS? (spreadable | hmethod | scope)
    ;

// Extended horizontal application tail
// Can contain elements in vertical notation
happlicationTailExtended
    : (SPACE (happlicationArgExtended | happlicationArgExtended as))+
    ;

// Extended argument of horizontal application
// Can contain elements in vertical notation
happlicationArgExtended
    : beginner
    | finisherCopied
    | DOTS? (spreadable | hmethodExtended | scopeExtended)
    | LB DOTS spreadable RB
    ;

// Vertical application
vapplication
    : vapplicationHeadNamed vapplicationArgs
    ;

// Vertical application head
vapplicationHead
    : applicable
    | hmethodExtended
    | hmethodExtendedVersioned
    | vmethod
    | vmethodVersioned
    | versioned
    ;

// Vertical application head with optional name
vapplicationHeadNamed
    : vapplicationHead oname?
    ;

// Vertical application arguments
vapplicationArgs
    : EOL
      TAB
      (
        ( vapplicationArgVanonym                                                // Vertical anonym object
        | vapplicationArgHanonym                                                // Horizontal anonym object
        | vapplicationArgHapplication oname?                                    // Horizontal application
        | (vapplicationHeadNamed | vapplicationHeadAs oname?) vapplicationArgs  // Vertical application
        | justNamed                                                             // Just an object reference
        | just as oname?                                                        // Just an object reference with binding
        | methodNamed                                                           // Method
        | methodAs oname?                                                       // Method with binding
        | vapplicationArgSpreadable                                             // Spreadable
        )
        (EOL | EOP)
      )+
      UNTAB
    ;

// Vertical application arguments that can be spread
vapplicationArgSpreadable
    : DOTS?
      ( just
      | just as
      | method
      | methodAs
      | vapplicationArgHapplication
      | vapplicationArgVapplication
      )
    ;

// Horizontal application as argument of vertical application
vapplicationArgHapplication
    : happlicationExtended
    | LB happlicationExtended RB as
    ;

// Vertical application as argument of vertical application
vapplicationArgVapplication
    : (vapplicationHead | vapplicationHeadAs) vapplicationArgs
    ;

// Vertical application head with binding
vapplicationHeadAs
    : (applicable | hmethodExtended | hmethodExtendedVersioned | versioned) as
    ;

// Vertical anonym object as argument of vertical application
vapplicationArgVanonym
    : attributes as? oname? abstractees?
    ;

// Horizontal anonym object as argument of vertical application
vapplicationArgHanonym
    : (hanonym | LB hanonym RB as) oname?
    ;

// Horizontal anonym object
hanonym
    : attributes hanonymInner+
    ;

// Inner object of horizontal anonym object
// Does not contan elements in vertical notation
hanonymInner
    : SPACE LB (hmethod | hmethodVersioned | happlication | hanonym | just) oname RB
    ;

// Abstract objects <- arguments of vertical anonym object <- argument of vertical application
abstractees
    : EOL
      TAB
      ((innerabstract | application | justNamed | methodNamed) (EOL | EOP))+
      UNTAB
    ;

// Inner abstract object of abstractees
innerabstract
    : ahead oname? abstractees?
    ;

// Optional comment + attributes
ahead
    : (COMMENT EOL)* attributes
    ;

// Method
method
    : hmethodExtended
    | hmethodExtendedVersioned
    | vmethod
    | vmethodVersioned
    ;

// Method with optional name
methodNamed
    : method oname?
    ;

// Method with bindning
methodAs
    : (hmethodExtended | hmethodExtendedVersioned) as
    ;

// Horizontal method
// The whole method is written in one line
// The head does not contain elements in vertical notation
hmethod
    : hmethodHead methodTail+
    ;

// Extended horizontal method
// The head can contain elements in vertical notation
hmethodExtended
    : hmethodHeadExtended methodTail+
    ;

// Versioned horizontal method
// The whole method is written in one line
// The head does not contain elements in vertical notation
// The division of elements into regular and versioned ones is due to
// the presence of horizontal application where head or agruments can't
// contain version
hmethodVersioned
    : hmethodHead methodTail* methodTailVersioned
    ;

// Versioned extended horizontal method
// The head can contain elements in vertical notation
hmethodExtendedVersioned
    : hmethodHeadExtended methodTail* methodTailVersioned
    ;

// Head of horizontal method
hmethodHead
    : beginner
    | finisherCopied
    | scope
    ;

// Extended head of horizontal method
hmethodHeadExtended
    : beginner
    | finisherCopied
    | scopeExtended
    ;

// Vertical method
vmethod
    : vmethodHead vmethodTail
    ;

// Vertical method with version
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
    : DOT finisherCopied
    ;

// Versioned tail of method
methodTailVersioned
    : DOT NAME version?
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
    | AT
    | RHO
    | SIGMA
    | VERTEX
    ;

// Something that can be spread
spreadable
    : (NAME | AT | RHO | SIGMA) COPY?
    ;

// Finisher with optional COPY
finisherCopied
    : finisher COPY?
    ;

// Name with optional version
versioned
    : NAME version?
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

// Suffix
suffix
    : SPACE ARROW SPACE (AT | NAME)
    ;

// Simple scope
// Does not contain elements in vertical notation
scope
    : LB (happlication | hmethod | hanonym) RB
    ;

// Extended scope
scopeExtended
    : LB (happlicationExtended | hmethodExtended | hanonym) RB
    ;

// Version
version
    : BAR VER
    ;

// Binding
as  : COLON (NAME | RHO)
    ;

// Data
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
