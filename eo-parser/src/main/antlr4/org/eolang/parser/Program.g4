grammar Program;

@header {
  import java.util.LinkedList;
}

tokens { TAB, UNTAB }

@lexer::members {
  private int currentTabs = 0;
  private LinkedList<Token> tokens = new LinkedList<>();
  @Override
  public Token nextToken() {
    return this.tokens.isEmpty() ? super.nextToken() : this.tokens.poll();
  }
  public void emitToken(int t, int line) {
    CommonToken tkn = new CommonToken(t, "");
    tkn.setLine(line);
    this.tokens.offer(tkn);
  }
}

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
    suffix?
  )*
  ;

abstraction
  :
  (COMMENT EOL)*
  LSQ
  (attribute (SPACE attribute)*)?
  RSQ
  (suffix (SPACE SLASH NAME)?)?
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
  (
    NAME
    |
    PARENT
  )
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
  LB
  application
  RB
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
    head
    |
    SPACE
    application
    method
    |
    SPACE
    LB
    application
    RB
    |
    SPACE
    application
    has
    |
    SPACE
    application
    suffix
  )+
  ;

head
  :
  AT
  |
  PARENT
  |
  SELF
  |
  NAME
  |
  NAME DOT
  |
  data
  ;

has
  :
  COLON
  NAME
  ;

data
  :
  BYTES
  |
  BOOL
  |
  STRING
  |
  INT
  |
  FLOAT
  |
  HEX
  |
  CHAR
  ;

COMMENT: HASH ~[\r\n]*;
META: PLUS NAME (SPACE ~[\r\n]+)?;

DOTS: '...';
CONST: '!';
SLASH: '/';
COLON: ':';
ARROW: '>';
SELF: '$';
PLUS: '+';
MINUS: '-';
SPACE: ' ';
DOT: '.';
LSQ: '[';
RSQ: ']';
LB: '(';
RB: ')';
AT: '@';
PARENT: '^';
HASH: '#';
EOL
  :
  [\r\n]+
  SPACE*
  {
    int tabs = getText().replaceAll("[\r\n]+", "").length() / 2;
    if (tabs < this.currentTabs) {
      for (int i = 0; i < this.currentTabs - tabs; ++i) {
        this.emitToken(ProgramParser.UNTAB, getLine() + 1);
        this.emitToken(ProgramParser.EOL, getLine() + 1);
      }
    } else if (tabs > this.currentTabs) {
      for (int i = 0; i < tabs - this.currentTabs; ++i) {
        this.emitToken(ProgramParser.TAB, getLine() + 1);
      }
    }
    this.currentTabs = tabs;
  }
  ;

fragment BYTE: [0-9A-F][0-9A-F] MINUS;
BYTES: BYTE (BYTE* [0-9A-F][0-9A-F])?;

BOOL: 'true' | 'false';
CHAR: '\'' [0-9a-zA-Z] '\'';
STRING: '"' ('\\"' | ~'"')* '"';
INT: (PLUS | MINUS)? [0-9]+;
FLOAT: (PLUS | MINUS)? [0-9]+ DOT [0-9]+;
HEX: '0x' [0-9a-f]+;

NAME: [a-z][a-z0-9_A-Z]*;

