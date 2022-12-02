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
  (COMMENT EOL)+
  ;

metas
  :
  (META EOL)+
  ;

objects
  :
  (
    (COMMENT EOL)*
    object
    EOL
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
  (attribute (SPACE attribute)*)?
  RSQ
  ;

attribute
  :
  label
  ;

label
  :
  AT
  |
  NAME
  DOTS?
  ;

tail
  :
  EOL
  TAB
  (object EOL)+
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
  htail?
  |
  application
  method
  htail?
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
    NAME
    DOT
    |
    data
    |
    abstraction
  )
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

COMMENT: HASH ~[\r\n]*;
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

fragment INDENT:
    SPACE SPACE
    ;
fragment LINEBREAK:
    ('\n' | '\r\n')
    ;
EOL
  :
  LINEBREAK
  LINEBREAK?
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
HEX: '0x' [0-9a-f]+;

NAME: [a-z][\p{Letter}\p{General_Category=Decimal_Number}_-]*;

fragment TEXT_MARK: '"""';
TEXT:
    TEXT_MARK ('\n' | '\r\n')
    (~[\\] | ESCAPE_SEQUENCE)*?
    TEXT_MARK
    ;
