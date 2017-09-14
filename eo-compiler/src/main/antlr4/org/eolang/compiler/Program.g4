grammar Program;

@header {
    import org.eolang.compiler.syntax.*;
    import org.eolang.compiler.syntax.Object;
    import org.eolang.compiler.syntax.Tree;
    import java.util.LinkedList;
    import java.util.Collections;
}

tokens { INDENT, DEDENT }

@lexer::members {
  // A queue where extra tokens are pushed on (see the NEWLINE lexer rule).
  private java.util.LinkedList<Token> tokens = new java.util.LinkedList<>();
  // The stack that keeps track of the indentation level.
  private java.util.Stack<Integer> indents = new java.util.Stack<>();
  // The amount of opened braces, brackets and parenthesis.
  private int opened = 0;
  // The most recently produced token.
  private Token lastToken = null;
  @Override
  public void emit(Token t) {
    super.setToken(t);
    tokens.offer(t);
  }

  @Override
  public Token nextToken() {
    // Check if the end-of-file is ahead and there are still some DEDENTS expected.
    if (_input.LA(1) == EOF && !this.indents.isEmpty()) {
      // Remove any trailing EOF tokens from our buffer.
      for (int i = tokens.size() - 1; i >= 0; i--) {
        if (tokens.get(i).getType() == EOF) {
          tokens.remove(i);
        }
      }

      // First emit an extra line break that serves as the end of the statement.
      this.emit(commonToken(ProgramParser.NEWLINE, "\n"));

      // Now emit as much DEDENT tokens as needed.
      while (!indents.isEmpty()) {
        this.emit(createDedent());
        indents.pop();
      }

      // Put the EOF back on the token stream.
      this.emit(commonToken(ProgramParser.EOF, "<EOF>"));
    }

    Token next = super.nextToken();

    if (next.getChannel() == Token.DEFAULT_CHANNEL) {
      // Keep track of the last token on the default channel.
      this.lastToken = next;
    }

    return tokens.isEmpty() ? next : tokens.poll();
  }

  private Token createDedent() {
    CommonToken dedent = commonToken(ProgramParser.DEDENT, "");
    dedent.setLine(this.lastToken.getLine());
    return dedent;
  }

  private CommonToken commonToken(int type, String text) {
    int stop = this.getCharIndex() - 1;
    int start = text.isEmpty() ? stop : stop - text.length() + 1;
    return new CommonToken(this._tokenFactorySourcePair, type, DEFAULT_TOKEN_CHANNEL, start, stop);
  }

  // Calculates the indentation of the provided spaces, taking the
  // following rules into account:
  //
  // "Tabs are replaced (from left to right) by one to eight spaces
  //  such that the total number of characters up to and including
  //  the replacement is a multiple of eight [...]"
  //
  //  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation
  static int getIndentationCount(String spaces) {
    int count = 0;
    for (char ch : spaces.toCharArray()) {
      switch (ch) {
        case '\t':
          count += 8 - (count % 8);
          break;
        default:
          // A normal space char.
          count++;
      }
    }

    return count;
  }

  boolean atStartOfInput() {
    return super.getCharPositionInLine() == 0 && super.getLine() == 1;
  }
}

program returns [Tree ret]
    :
    { List<RootNode> nodes = new LinkedList<>(); }
    (
        NEWLINE
        |
        root_stmt
        { nodes.add($root_stmt.ret); }
    )*
    { $ret = new Tree(nodes); }
    EOF
    ;

root_stmt returns [RootNode ret]
    :
    obj_def
    { $ret = $obj_def.ret; }
    |
    type_def
    { $ret = $type_def.ret; }
    ;

type_def returns [RootNode ret]
    :
    { List<Method> methods = new LinkedList<Method>(); }
    TYPE
    SPACE
    type_name
    (
        SPACE
        EXTENDS
        SPACE
        HINAME
        (
            COMMA
            SPACE
            HINAME
        )*
    )?
    COLON
    NEWLINE
    INDENT
    (
        method_sign
        { methods.add($method_sign.ret); }
        NEWLINE
    )+
    DEDENT
    { $ret = new Type($type_name.ret, methods); }
    ;

obj_suite returns [ObjectBody ret]
    :
    NEWLINE
    {
        List<Attribute> attrs = new LinkedList<>();
        List<Ctor> ctors = new LinkedList<>();
        List<MethodImpl> methods = new LinkedList<>();
    }
    INDENT
    (
        method_def
        { methods.add($method_def.ret); }
        |
        attr_def
        { attrs.add($attr_def.ret); }
        |
        ctor_def
        { ctors.add($ctor_def.ret); }
    )+
    { $ret = new ObjectBody(attrs, ctors, methods); }
    DEDENT;

ctor_suite returns [List<Argument> ret]
    :
    NEWLINE
    INDENT
    ctor_stmt
    { $ret = $ctor_stmt.ret; }
    DEDENT
    ;

method_suite returns [Argument ret]
    :
    NEWLINE
    INDENT
    method_stmt
    DEDENT
    { $ret = $method_stmt.ret; }
    ;

cp_suite returns [List<Argument> ret]
    :
    { List<Argument> args = new LinkedList<>(); }
    NEWLINE
    INDENT
    (
        cp_arg
        { args.add($cp_arg.ret); }
    )+
    DEDENT
    { $ret = args; }
    ;

ctor_stmt returns [List<Argument> ret]
    :
    cp_stmt
    { $ret = $cp_stmt.ret.arguments(); }
    ;

method_stmt returns [Argument ret]
    :
    cp_arg
    { $ret = $cp_arg.ret; }
    ;

cp_stmt returns [CpObject ret]
    :
    obj_name
    COLON
    cp_suite
    { $ret = new CpObject($obj_name.ret, $cp_suite.ret); }
    ;

cp_arg returns [Argument ret]
  :
    scalar
    { $ret = $scalar.ret; }
    NEWLINE
    |
    param_name
    { $ret = new ArgParam($param_name.ret); }
    NEWLINE
    |
    cp_stmt
    { $ret = new ArgCpObject($cp_stmt.ret); }
    ;

attr_def returns [Attribute ret]
  :
    type_name
    SPACE
    attr_name
    NEWLINE
    { $ret = new Attribute($type_name.ret, $attr_name.ret); }
    ;

obj_def returns [Object ret]
    :
    { List<String> types = new LinkedList<>(); }
    OBJECT
    SPACE
    obj_name
    SPACE
    AS
    SPACE
    type_name
    { types.add($type_name.ret); }
    (
        COMMA
        SPACE
        type_name
        { types.add($type_name.ret); }
    )*
    COLON
    obj_suite
    { $ret = new Object($obj_name.ret, types, $obj_suite.ret); }
    ;

ctor_def returns [Ctor ret]
    :
    CTOR
    params_def
    COLON
    ctor_suite
    { $ret = new Ctor($params_def.ret, $ctor_suite.ret); }
    ;

method_def returns [MethodImpl ret]
    :
    type_name
    SPACE
    method_name
    params_def
    COLON
    method_suite
    { $ret = new MethodImpl(new Method($method_name.ret, $params_def.ret, $type_name.ret), $method_suite.ret); }
    ;

method_sign returns [Method ret]
    :
    type_name
    SPACE
    method_name
    params_def
    { $ret = new Method($method_name.ret, $params_def.ret, $type_name.ret); }
    ;

params_def returns [List<Parameter> ret]
    :
    { List<Parameter> params = new LinkedList<>(); }
    LBRACKET
    (
        (
            param_def
            { params.add($param_def.ret); }
            COMMA
            SPACE
        )*
        (
            param_def
            { params.add($param_def.ret); }
        )
    )?
    RBRACKET
    { $ret = params; }
    ;

obj_name returns [String ret]
  :
    LONAME
    { $ret = $LONAME.text; }
    ;

type_name returns [String ret]
  :
    HINAME
    { $ret = $HINAME.text; }
    ;

method_name returns [String ret]
  :
    LONAME
    { $ret = $LONAME.text; }
    ;

attr_name returns [String ret]
  :
    ATTRIBUTE
    { $ret = $ATTRIBUTE.text.substring(1); }
    ;


scalar returns [Argument ret]
  :
    NUMBER
    { $ret = new ArgSimple($NUMBER.text); }
    |
    TEXT
    { $ret = new ArgSimple($TEXT.text); }
    |
    ATTRIBUTE
    { $ret = new ArgAttribute($ATTRIBUTE.text.substring(1)); }
    ;

param_def returns [Parameter ret]
  :
    type_name
    SPACE
    param_name
    { $ret = new Parameter($param_name.text, $type_name.text); }
    ;

param_name returns [String ret]
  :
    LONAME
    { $ret = $LONAME.text; }
    ;


SPACE: ' ';
DOT: '.';
COLON: ':';
LBRACKET: '(';
RBRACKET: ')';
COMMA: ',';

TYPE: 'type';
OBJECT: 'object';
EXTENDS: 'extends';
AS: 'as';
CTOR: 'ctor';

ATTRIBUTE: '@' ( 'a' .. 'z' ) LETTER*;
HINAME: ( 'A' .. 'Z' ) LETTER*;
LONAME: ( 'a' .. 'z' ) LETTER*;
NUMBER: ( '0' .. '9' )+;
LETTER: ( 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' );
TEXT: '"' ('\\"' | ~'"')* '"';

fragment SPACES: [ \t]+;

NEWLINE
 : ( {atStartOfInput()}?   SPACES
   | ( '\r'? '\n' | '\r' | '\f' ) SPACES?
   )
   {
     String newLine = getText().replaceAll("[^\r\n\f]+", "");
     String spaces = getText().replaceAll("[\r\n\f]+", "");
     int next = _input.LA(1);
     if (opened > 0 || next == '\r' || next == '\n' || next == '\f' || next == '#') {
       // If we're inside a list or on a blank line, ignore all indents,
       // dedents and line breaks.
       skip();
     }
     else {
       emit(commonToken(NEWLINE, newLine));
       int indent = getIndentationCount(spaces);
       int previous = indents.isEmpty() ? 0 : indents.peek();
       if (indent == previous) {
         // skip indents of the same size as the present indent-size
         skip();
       }
       else if (indent > previous) {
         indents.push(indent);
         emit(commonToken(ProgramParser.INDENT, spaces));
       }
       else {
         // Possibly emit more than 1 DEDENT token.
         while(!indents.isEmpty() && indents.peek() > indent) {
           this.emit(createDedent());
           indents.pop();
         }
       }
     }
   }
;