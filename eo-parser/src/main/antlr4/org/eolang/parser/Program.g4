grammar Program;

@header {
  import java.util.LinkedList;
}

tokens {
	TAB,
	UNTAB
}

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

program: license? metas? objects EOF;

license: (COMMENT EOL)+;

metas: (META EOL)+;

objects: ( (COMMENT EOL)* object EOL)+;

object:
	anonymous
	| (abstraction | application) tail? (
		EOL method htail? suffix? tail?
	)*;

anonymous: attributes htail;

abstraction:
	(COMMENT EOL)* attributes (
		suffix (SPACE SLASH (NAME | QUESTION))?
	)?;

attributes:
	LSQ (
		// starts with @ | @? is somewhere btw names ends with dots?
		(AT | NAME ( SPACE NAME)* ( SPACE AT)?) (SPACE NAME)* DOTS?
	)? RSQ;

tail: EOL TAB (object EOL)+ UNTAB;

suffix: SPACE ARROW SPACE NAME CONST?;

method: DOT mtd = ( NAME | RHO | AT | VERTEX);

application:
	head htail?
	| application method htail?
	| LB application RB htail?
	| application has htail?
	| application suffix htail?;

htail:
	(
		SPACE head
		| SPACE application method
		| SPACE LB application RB
		| SPACE application has
		| SPACE application suffix
		| SPACE anonymous
	)+;

head:
	DOTS? (
		ROOT
		| AT
		| RHO
		| XI
		| SIGMA
		| STAR
		| NAME COPY?
		| NAME DOT
		| data
	);

has: COLON NAME;

data:
	BYTES
	| BOOL
	| TEXT
	| STRING
	| INT
	| FLOAT
	| HEX
	| CHAR
	| REGEX;

COMMENT: HASH ~[\r\n]*;
META: PLUS NAME (SPACE ~[\r\n]+)?;

REGEX: SLASH ~[\r\n]+ SLASH [a-z]*;

ROOT: 'Q';
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
EOL:
	('\n' | '\r\n') ('\n' | '\r\n')? SPACE* {
    int tabs = getText().replaceAll("[\r]?[\n]", "").length() / 2;
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
  };

fragment BYTE: [0-9A-F][0-9A-F];
fragment EMPTY_BYTES: MINUS MINUS;
fragment LINE_BYTES: BYTE (MINUS BYTE)+;

BYTES:
	EMPTY_BYTES
	| BYTE MINUS
	| LINE_BYTES (MINUS EOL LINE_BYTES)*;

BOOL: 'TRUE' | 'FALSE';
CHAR: '\'' (~['\\\r\n] | ESCAPE_SEQUENCE) '\'';
STRING: '"' (~["\\\r\n] | ESCAPE_SEQUENCE)* '"';

fragment ESCAPE_SEQUENCE:
	'\\' [btnfr"'\\]
	// allowed numbers 000.255 (ASCII range)
	| '\\' ([01][0-9][0-9] | '2' [0-4][0-9] | '25' [0-5])
	| '\\' 'u'+ BYTE BYTE;

// no leading zeros allowed
INT: (PLUS | MINUS)? ([1-9][0-9]* | [0]);
FLOAT: INT DOT [0-9]+;
HEX: '0x' [0-9a-f]+;

NAME: [a-z][\p{Letter}\p{General_Category=Decimal_Number}_-]*;

fragment TEXT_MARK: '"""';
TEXT:
	TEXT_MARK ('\n' | '\r\n') (~[\\] | ESCAPE_SEQUENCE)*? TEXT_MARK;