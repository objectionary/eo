<img src="http://cf.jare.io/?u=http%3A%2F%2Fwww.yegor256.com%2Fimages%2Fbooks%2Felegant-objects%2Fcactus.svg" height="100px" />

[![DevOps By Rultor.com](http://www.rultor.com/b/yegor256/eo)](http://www.rultor.com/p/yegor256/eo)

[![Build Status](https://travis-ci.org/yegor256/eo.svg?branch=master)](https://travis-ci.org/yegor256/eo)
[![PDD status](http://www.0pdd.com/svg?name=yegor256/eo)](http://www.0pdd.com/p?name=yegor256/eo)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/yegor256/takes/blob/master/LICENSE.txt)

**EO** (stands for [Elegant Objects](http://www.yegor256.com/elegant-objects.html) or
ISO 639-1 code of [Esperanto](https://en.wikipedia.org/wiki/Esperanto))
is an object-oriented programming language. It's still a prototype.
It's the future of OOP. Please contribute!

If you want to contribute, please join our
[Gitter chat](https://gitter.im/yegor256/elegantobjects) first.

Our Twitter tag is [#eolang](https://twitter.com/search?q=%23eolang).

These things we **don't tolerate**:

  * static/class methods or attributes ([why?](http://www.yegor256.com/2014/05/05/oop-alternative-to-utility-classes.html))
  * classes ([why?](http://www.yegor256.com/2016/09/20/oop-without-classes.html))
  * implementation inheritance ([why?](http://www.yegor256.com/2016/09/13/inheritance-is-procedural.html))
  * mutability ([why?](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html))
  * NULL ([why?](http://www.yegor256.com/2014/05/13/why-null-is-bad.html))
  * global variables/procedures
  * reflection
  * constants
  * type casting ([why?](http://www.yegor256.com/2015/04/02/class-casting-is-anti-pattern.html))
  * scalar types
  * garbage collection ([huh?](https://github.com/yegor256/eo/issues/4))
  * annotations ([why?](http://www.yegor256.com/2016/04/12/java-annotations-are-evil.html))
  * unchecked exceptions ([why?](http://www.yegor256.com/2015/07/28/checked-vs-unchecked-exceptions.html))
  * operators
  * flow control statements (`for`, `while`, `if`, etc)
  * DSL and [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) ([why?](https://github.com/yegor256/eo/issues/51))

These things we **want** to build in:

  * static analysis
  * continuous integration
  * build automation
  * aspects (AOP)
  * logging
  * TDD ([discuss](https://github.com/yegor256/eo/issues/34))
  * versioning ([discuss](https://github.com/yegor256/eo/issues/22))
  * concurrency
  * object metadata
  * persistence
  * transactions
  * licensing
  * artifact repositories

These things we are **not sure** about (please, help us):

  * we don't need generics ([not sure](https://github.com/yegor256/eo/issues/1))

We want EO to be compilable to Java. We want to stay as close to Java and JVM
as possible, mostly in order to re-use the eco-system and libraries
already available.

We also want to have an ability to compile it to any other language, like
Python, C/C++, Ruby, C#, etc. In other words, EO must be platform
independent.

## Quick Start

Here is a classic "hello, world" example:

```
import org.eolang.cli
import org.eolang.printed
import org.eolang.string
cli:
  printed:
    string:
      "Hello, world!"
```

This code will compile into a `.java` class that will compile into
a `.class` byte code that will run and print "Hello, world!".

What exactly happens here? TBD

## Details

There are types, objects, constructors, destructors, methods,
attributes, arguments, exceptions, decorators.

There are _no_ classes, variables, operators, statements, annotations, reflection,
type casting, generics, NULL, static methods, functions, lambdas.

### Types

A type is a _contract_ an object must obey.

A type is very similar to interfaces from Java 1.0. A type has
a name and a list of method
declarations. Each method declaration has a name of the type of
the return value, a method name, and a list of arguments. Each method
declaration has to take exactly one line, without any terminating
symbol at the end, for example:

```
type Book:
  Text asText()
type Car:
  Money cost()
  Bytes picture()
  Car moveTo(Coordinates coords)
type Pixel extends Point:
  Pixel moveTo(Int x, Int y)
  Bytes drawTo(Canvas canvas)
```

There is only one built-in type `Bytes`, which is very similar
to `byte[]` in Java.

A type may become a [subtype](https://en.wikipedia.org/wiki/Subtyping)
of another type by using keyword `extends`.

A type may have one to four method declarations.

The type name must match `[A-Z][A-Za-z0-9]{2,15}`.

### Objects

An object is an active entity that implements one or more types.

An object can be _created_ or _copied_. When an object is created
all methods required by its types must be implemented, for example
(`alphabet` is the name of the object):

```
object alphabet as Book:
  Text @isbn
  Text @title
  ctor()
    alphabet:
      "978-1-51916-691-3",
      "The Alphabet"
  ctor(String i, String t):
    @isbn = i
    @title = t
  Text asText():
    sprintf:
      "ISBN is %s, title is '%s'",
      @isbn,
      @title
```

An object can be made as a copy of an existing object, but with a
different (or similar) set of arguments for one of the constructors, for
example (`ot` is the name of the object):

```
ot: "978-0-73561-965-4", "Object Thinking"
```

The same can be written in a few lines (comma at the end is mandatory):

```
ot:
  "978-0-73561-965-4",
  "Object Thinking"
```

Object creating and copying may be combined in any possible way, for example:

```
Ticket ticket(Person passenger):
  object as Ticket:
    Person @p
    ctor(Passenger p)
    Text name():
      concat:
        "506-",
        @p.name()
    Money price():
      if:
        @p.vip(),
        money("$50"),
        object () as Money
          Int value():
            25
```

The object name must match `[a-z][a-z0-9]{2,15}`.

### Attributes

An attribute is the coordinate of an encapsulated object.

An object may have a number of attributes, listed right after the
first line of object declaration:

```
object zero(0, "USD") as Money:
  Int @amount
  Text @currency
```

All attributes are private; there is no such thing as public or protected
attributes.

An object may have up to five attributes.

The attribute name must match `@[a-z][a-z0-9]{2,15}`.

### Constructors and Destructors

A constructor is a ... TBD

A destructor is a ... TBD

An object must have a primary constructor (which must have at least
one parameter, i.e. the object must encapsulate something) and may have
any number of secondary constructors and one destructor.
A primary constructor is the one that initializes object
attributes and can't have a body, for example:

```
object zero() as Money, Int:
  Int @amount
  Text @currency
  ctor(): # secondary constructor
    zero: 0
  ctor(Int a): # secondary constructor
    zero: a, "USD"
  ctor(Int amount, Text currency) # primary constructor
  dtor(): # destructor
    printed: "I'm dying..."
```

Obviously, parameter names in the primary constructor must match
the names of object attributes.

Secondary constructors must return objects that implement at least
all types mentioned in the declaration of the current object.

The destructor, if it is present in the object, will be called right
before the object is destroyed. The object returned by the
destructor is activated immediately after being returned.

Constructors must be listed after attributes. The primary constructor
must be the last one. The destructor, if it is present, goes
right after the primary constructor.

For constructors the `ctor` keyword is used and for destructors,
the keyword is `dtor`.

### Methods

A method is a behavior exposing instrument of an object.

Method body must create and return exactly one object, using a single
"statement", for example:

```
Int max(Int a, Int b):
  if:
    firstIsLess:
      a,
      b
    b,
    a
```

A method must return something; there is no such thing as `void` like there is in Java/C++.

A method may not exist in an object if it is not defined
in one of its types.

All methods are public; there is no such thing as private or protected methods.

A method may have zero to four arguments.

Each argument must have an optional type and a name. If type is not provided
it has to be replaced with `?` mark, for example:

```
Int power(? x, Int p):
  if:
    equals: p, 0,
    1,
    mul:
      x,
      power: x, minus: p, 1
```

This code will lead to compile-time error if `x` doesn't implement `Int`.

The method name must match `[a-z][a-z0-9]{2,15}`.

A single empty line is allowed after the method body, nowhere else.

### Exceptions

To raise an exception object `org.eolang.error` must be copied (imported by
default):

```
import org.eolang.error # you can omit this line
Int price(Int total, Int amount):
  if:
    equals: amount, 0
    error: "Division by zero"
    div: total, amount
```

To catch an exception... TBD

## Popular Types and Objects

Some types are the most popular:

```
Int
Boolean
Double
String
```

Some objects are popular as well:

```
if
equals as Boolean
not as Boolean
firstIsLess as Boolean
firstIsGreater as Boolean
plus
minus
mul
div
```

## Examples

A Fibonacci number:

```
object fibonacci(1) as Int:
  Int @n
  ctor():
    fibonacci: 1
  ctor(Int n)
  Int int():
    if:
      firstIsLess: @n, 2
      1,
      plus:
        @n
        fibonacci:
          minus: @n, 1
```

Here, `if`, `firstIsLess`, `plus`, and `minus` are objects being copied.

# How to contribute

Fork repository, make changes, send us a pull request. We will review your changes and apply them to the master branch shortly, provided they don't violate our quality standards. To avoid frustration, before sending us your pull request please run full Maven build:

`$ mvn clean install -Pqulice`

# Troubleshooting
## Intellij IDEA
If after clean import from Maven, IDEA doesn't compile the project
and shows errors about ProgramLexer is undefined:
- Open Maven Projects window
- Select eo-compiler
- Right button
- Generate Sources and Update Folders

# License

* Copyright (c) 2016 eolang.org [contributors](https://github.com/yegor256/eo/graphs/contributors)

This EO documentation, examples and the [eo-compiler](eo-compiler)
is distributed under [The MIT License](http://www.opensource.org/licenses/MIT).
See [LICENSE](LICENSE) for details.
