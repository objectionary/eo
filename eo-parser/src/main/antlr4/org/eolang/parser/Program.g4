grammar Program;

tokens { TAB, UNTAB }

program
    : license? metas? objects EOF
    ;

license
    : (COMMENT EOL)* COMMENT EOP
    ;

metas
    : (META EOL)* META EOP
    ;

objects
    : ((COMMENT EOL)* object (EOL | EOP))+
    ;

object
    : atom
    | abstraction
    | application
    | methodNamed
    | justNamed
    ;

just: beginner
    | finisherOrCopy
    | versioned
    ;

justNamed
    : just oname?
    ;

justHas
    : just has
    ;

justHasNamed
    : justHas oname?
    ;

atom: ahead suffix type
    ;

abstraction
    : ahead suffix inners?
    ;

inners
    : EOL TAB (object (EOL | EOP))+ UNTAB
    ;

attributes
    : LSQ
      ((attribute (SPACE attribute)* (SPACE vararg)?)? | vararg)
      RSQ
    ;

attribute
    : NAME
    ;

type: SPACE SLASH (NAME | QUESTION)?
    ;

vararg
    : NAME DOTS
    ;

application
    : happlicationNamed
    | vapplication
    ;

happlication
    : happlicationHead happlicationTail
    ;

happlicationNamed
    : happlication oname?
    ;

happlicationHead
    : vmethod
    | hmethod
    | applicable
    ;

applicable
    : STAR
    | (NAME | AT) COPY?
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

vapplication
    : vapplicationHeadNamed vapplicationArgs
    ;

vapplicationHead
    : applicable
    | hmethod
    | hmethodVersioned
    | vmethod
    | vmethodVersioned
    | reversed
    | versioned
    ;

vapplicationHeadNamed
    : vapplicationHead oname?
    ;

vapplicationArgs
    : EOL
      TAB
      (
        ( vapplicationArgAbstract
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
    : happlication
    | LB happlication RB has
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
    : (applicable | hmethod | hmethodVersioned | reversed | versioned) has
    ;

vapplicationHeadHasNamed
    : vapplicationHeadHas oname?
    ;

vapplicationArgAbstract
    : attributes has? suffix? abstractees?
    ;

abstractees
    : EOL
      TAB
      ((innerabstract | application | justNamed | methodNamed) (EOL | EOP))+
      UNTAB
    ;

innerabstract
    : ahead suffix? abstractees?
    ;

ahead
    : (COMMENT EOL)* attributes
    ;

method
    : hmethod
    | hmethodVersioned
    | vmethod
    | vmethodVersioned
    ;

methodNamed
    : method oname?
    ;

methodHas
    : (hmethod | hmethodVersioned) has
    ;

methodHasNamed
    : methodHas oname?
    ;

hmethod
    : hmethodHead methodTail+
    ;

hmethodVersioned
    : hmethodHead methodTail* methodTailVersioned
    ;

hmethodHead
    : beginner
    | finisherOrCopy
    | scope
    ;

vmethod
    : vmethodHead vmethodTail
    ;

vmethodVersioned
    : vmethodHead vmethodTailVersioned
    ;

vmethodHead
    : vmethodHead (vmethodTail | vmethodTailVersioned) oname?
    | hmethod oname?
    | hmethodVersioned oname?
    | vmethodHead (vmethodTail | vmethodTailVersioned) oname? vapplicationArgs oname?
    | (applicable | hmethod | hmethodVersioned | reversed | versioned) oname? vapplicationArgs oname?
    | vmethodHead (vmethodTail | vmethodTailVersioned) happlicationTail oname?
    | (applicable | hmethod) happlicationTail oname?
    | justNamed
    ;

vmethodTail
    : EOL methodTail
    ;

vmethodTailVersioned
    : EOL methodTailVersioned
    ;

methodTail
    : DOT mtd
    ;

methodTailVersioned
    : DOT mtdVersioned
    ;

beginner
    : STAR
    | ROOT
    | HOME
    | XI
    | data
    ;

finisher
    : NAME
    | AT
    | RHO
    | SIGMA
    | VERTEX
    ;

spreadable
    : (NAME | AT | RHO | SIGMA) COPY?
    ;

mtd : finisherOrCopy
    ;

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
    : LB (happlication | hmethod) RB
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
