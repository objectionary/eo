grammar Program;

tokens { TAB, UNTAB }

program
  :
  license?
  metas?
  objects
  EOF
  ;

license
  :
  (COMMENT EOL)*
  COMMENT EOP
  ;

metas
  :
  (META EOL)*
  META EOP
  ;

objects
  :
  (
    (COMMENT EOL)*
    object
    (EOL | EOP)
  )+
  ;

object
  :
  (
    abstraction
    |
    application
  )
  tail?
  (
    EOL
    method
    htail?
    suffix?
    tail?
  )*
  ;

abstraction
  :
  (COMMENT EOL)*
  attributes
  (
    (suffix (SPACE SLASH (NAME | QUESTION))?)
    | htail
  )?
  ;

attributes
  :
  LSQ
  (
    (attribute (SPACE attribute)* (SPACE vararg)?)?
    |
    vararg
  )
  RSQ
  ;

attribute
  :
  label
  ;

vararg
  :
  label
  DOTS
  ;

label
  :
  AT
  |
  NAME
  ;

tail
  :
  EOL
  TAB
  (object (EOL | EOP))+
  UNTAB
  ;

suffix
  :
  SPACE
  ARROW
  SPACE
  label
  CONST?
  ;

method
  :
  DOT
  mtd=(
    NAME
    |
    RHO
    |
    SIGMA
    |
    AT
    |
    VERTEX
  )
  COPY?
  ;

scope
 :
 LB
 application
 RB
;

application
  :
  head
  version
  |
  head
  htail?
  |
  application
  method
  htail?
  |
  application
  method
  version
  suffix?
  |
  scope
  htail?
  |
  application
  has
  htail?
  |
  application
  suffix
  htail?
  ;

htail
  :
  (
    SPACE
    application
    method
    |
    SPACE
    head
    |
    SPACE
    scope
    |
    SPACE
    application
    has
    |
    SPACE
    application
    suffix
    |
    SPACE
    abstraction
  )+
  ;

head
  :
  DOTS?
  (
    ROOT
    |
    HOME
    |
    (
      AT
      |
      RHO
      |
      XI
      |
      SIGMA
    )
    DOT?
    |
    STAR
    |
    NAME
    COPY?
    |
    (
      NAME
      |
      VERTEX
    )
    DOT
    |
    data
    |
    abstraction
  )
  ;

version
  :
  BAR
  VER
  ;

has
  :
  COLON
  (
    NAME
    |
    RHO
  )
  ;

data
  :
  BYTES
  |
  BOOL
  |
  TEXT
  |
  STRING
  |
  INT
  |
  FLOAT
  |
  HEX
  ;

COMMENT: HASH | (HASH ~[\r\n]* ~[\r\n\t ]);
META: PLUS NAME (SPACE ~[\r\n]+)?;

ROOT: 'Q';
HOME: 'QQ';
STAR: '*';
DOTS: '...';
CONST: '!';
SLASH: '/';
COLON: ':';
COPY: '\'';
ARROW: '>';
VERTEX: '<';
SIGMA: '&';
XI: '$';
PLUS: '+';
MINUS: '-';
QUESTION: '?';
SPACE: ' ';
DOT: '.';
LSQ: '[';
RSQ: ']';
LB: '(';
RB: ')';
AT: '@';
RHO: '^';
HASH: '#';
BAR: '|';

fragment INDENT:
    SPACE SPACE
    ;
fragment LINEBREAK:
    ('\n' | '\r\n')
    ;

EOL
  :
  LINEBREAK
  INDENT*
  ;

EOP
  :
  LINEBREAK
  LINEBREAK
  INDENT*
  ;

fragment BYTE: [0-9A-F][0-9A-F];
fragment EMPTY_BYTES : MINUS MINUS;
fragment LINE_BYTES : BYTE (MINUS BYTE)+;

BYTES:
       EMPTY_BYTES
    |  BYTE MINUS
    |  LINE_BYTES (MINUS EOL LINE_BYTES)*;

BOOL: 'TRUE' | 'FALSE';
STRING: '"' (~["\\\r\n] | ESCAPE_SEQUENCE)* '"';

fragment ESCAPE_SEQUENCE
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ BYTE BYTE
    ;

fragment ZERO:
    '0';

INT: (PLUS | MINUS)? (ZERO | ZERO?[1-9][0-9]*);

fragment EXPONENT: ('e'|'E') (PLUS | MINUS)? ('0'..'9')+;
FLOAT: (PLUS | MINUS)? [0-9]+ DOT [0-9]+ EXPONENT?;
HEX: '0x' [0-9a-fA-F]+;

NAME: [a-z] ~[ \r\n\t,.|':;!?\][}{)(]*;
VER: [0-9]+ DOT [0-9]+ DOT [0-9]+;

fragment TEXT_MARK: '"""';
TEXT:
    TEXT_MARK ('\n' | '\r\n')
    (~[\\] | ESCAPE_SEQUENCE)*?
    TEXT_MARK
    ;
