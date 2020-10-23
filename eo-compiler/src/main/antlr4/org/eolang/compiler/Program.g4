grammar Program;

@header {
    import org.xembly.Directive;
    import org.xembly.Directives;
    import java.util.LinkedList;
    import java.util.Collection;
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

program returns [Iterable<Directive> ret]
    :
    header?
    metas?
    object
    EOL
    { $ret = new Directives(); }
    ;

header
    :
    (COMMENT EOL)+
    EOL
    ;

metas returns [Iterable<Directive> ret]
    :
    { Directives dirs = new Directives(); }
    (
        META
        { dirs.add("meta").set($META.text).up(); }
        EOL
    )+
    EOL
    { $ret = dirs; }
    ;

object returns [Iterable<Directive> ret]
    :
    COMMENT*
    (
        abstraction
        { $ret = $abstraction.ret; }
        |
        application
        { $ret = $application.ret; }
    )
    ;

abstraction returns [Iterable<Directive> ret]
    :
    (
        NAME
        (
            SPACE
            attributes
        )?
        SPACE
    )?
    EQ
    kids
    { $ret = new Directives(); }
    ;

attributes returns [Iterable<Directive> ret]
    :
    { Directives dirs = new Directives(); }
    LB
    NAME
    { dirs.add("attribute").set($NAME.text).up(); }
    (
        SPACE
        NAME
        { dirs.add("attribute").set($NAME.text).up(); }
    )*
    RB
    { $ret = dirs; }
    ;

kids returns [Iterable<Directive> ret]
    :
    { Directives dirs = new Directives(); }
    EOL
    TAB
    object
    { dirs.append($object.ret); }
    (
        EOL
        object
        { dirs.append($object.ret); }
    )*
    { $ret = dirs; }
    ;

application returns [Iterable<Directive> ret]
    :
    (
        NAME
        SPACE
        EQ
        SPACE
    )?
    (
        term
        |
        NAME
        kids
    )
    { $ret = new Directives(); }
    ;

term returns [Iterable<Directive> ret]
    :
    NAME
    |
    primitive
    |
    term
    DOT
    NAME
    |
    AT
    |
    term
    (
        SPACE
        term
    )+
    |
    LB
    term
    RB
    { $ret = new Directives(); }
    ;

primitive returns [Iterable<Directive> ret]
    :
    (
        STRING
        |
        INTEGER
        |
        FLOAT
        |
        HEX
        |
        CHAR
    )
    { $ret = new Directives(); }
    ;

EQ: '=';
PLUS: '+';
SPACE: ' ';
DOT: '.';
LB: '(';
RB: ')';
AT: '@';
HASH: '#';
EOL
    :
    [\r\n]
    SPACE*
    {
        int tabs = getText().replaceAll("[\r\n]+", "").length() / 2;
        if (tabs < this.currentTabs) {
            for (int i = 0; i < this.currentTabs - tabs; ++i) {
                this.emitToken(ProgramParser.UNTAB, getLine() + 1);
            }
        } else if (tabs > this.currentTabs) {
            for (int i = 0; i < tabs - this.currentTabs; ++i) {
                this.emitToken(ProgramParser.TAB, getLine() + 1);
            }
        }
        this.currentTabs = tabs;
    }
    ;

NAME: LETTER (LETTER | DIGIT)*;

CHAR: '\'' () '\'';
STRING: '"' ('\\"' | ~'"')* '"';
INTEGER: (PLUS | '-')? DIGIT+;
FLOAT: (PLUS | '-')? DIGIT+ DOT DIGIT+;
HEX: '0x' DIGIT+;

LETTER: [a-zA-Z];
DIGIT: [0-9];

COMMENT: HASH ~[\r\n]*;
META: PLUS NAME (SPACE ~[\r\n]+)?;
