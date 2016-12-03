grammar Program;

@header {
    import org.eolang.compiler.syntax.Argument;
    import org.eolang.compiler.syntax.Method;
    import org.eolang.compiler.syntax.Tree;
    import org.eolang.compiler.syntax.Type;
    import java.util.Collection;
    import java.util.LinkedList;
}

program returns [Tree ret]
    :
    { Collection<Type> types = new LinkedList<Type>(); }
    (
        type_declaration
        { types.add($type_declaration.ret); }
        |
        object_instantiation
        |
        object_copying
    )*
    EOF
    { $ret = new Tree(types); }
    ;

type_declaration returns [Type ret]
    :
    { Collection<Method> methods = new LinkedList<Method>(); }
    TYPE
    SPACE
    HINAME
    (
        SPACE
        EXTENDS
        HINAME
        (
            COMMA
            SPACE
            HINAME
        )*
    )?
    COLON
    EOL
    (
        SPACE+
        method_declaration
        { methods.add($method_declaration.ret); }
        EOL
    )+
    { $ret = new Type($HINAME.text, methods); }
    ;

method_declaration returns [Method ret]
    :
    HINAME
    SPACE
    LONAME
    arguments_declaration
    { $ret = new Method($LONAME.text, $arguments_declaration.ret, $HINAME.text); }
    ;

arguments_declaration returns [List<Argument> ret]
    :
    { $ret = new LinkedList<Argument>(); }
    LBRACKET
    (
        head=argument_declaration
        { $ret.add($head.ret); }
        (
            COMMA
            tail=argument_declaration
            { $ret.add($tail.ret); }
        )*
    )?
    RBRACKET
    ;

argument_declaration returns [Argument ret]
    :
    HINAME
    SPACE
    LONAME
    { $ret = new Argument($LONAME.text, $HINAME.text); }
    ;

object_instantiation
    :
    OBJECT
    SPACE
    LONAME
    SPACE
    AS
    SPACE
    HINAME
    (
        COMMA
        SPACE
        HINAME
    )*
    COLON
    EOL
    (
        SPACE+
        attribute_declaration
        EOL
    )+
    (
        SPACE+
        ctor_declaration
        EOL
    )+
    (
        SPACE+
        method_implementation
        EOL
    )*
    ;

attribute_declaration
    :
    HINAME
    SPACE
    ATTRIBUTE
    ;

ctor_declaration
    :
    CTOR
    arguments_declaration
    COLON
    EOL
    SPACE+
    (
        object_instantiation
        |
        object_copying
    )
    ;

method_implementation
    :
    method_declaration
    COLON
    SPACE+
    (
        object_instantiation
        |
        object_copying
    )
    EOL
    ;

object_copying
    :
    LONAME
    (
        COLON
        EOL
        SPACE+
        object_argument
        (
            EOL
            SPACE+
            object_argument
        )*
    )?
    ;

object_argument
    :
    NUMBER
    |
    TEXT
    |
    ATTRIBUTE
    |
    object_copying
    |
    object_instantiation
    ;

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

SPACE: ' ';
EOL: '\n';
