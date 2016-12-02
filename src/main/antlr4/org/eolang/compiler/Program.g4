/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
grammar Program;

@header {
    import org.eolang.compiler.syntax.Argument;
    import org.eolang.compiler.syntax.Method;
    import org.eolang.compiler.syntax.Tree;
    import org.eolang.compiler.syntax.Type;
}

tree returns [Tree ret]
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
        { $methods.add($method_declaration.ret); }
        EOL
    )+
    { $ret = new Type($TYPE.text, methods); }
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
    { $ret = new Argument($HINAME.text, $LONAME.text); }
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
