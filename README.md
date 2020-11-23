<img src="http://cf.jare.io/?u=http%3A%2F%2Fwww.yegor256.com%2Fimages%2Fbooks%2Felegant-objects%2Fcactus.svg" height="100px" />

[![Donate via Zerocracy](https://www.0crat.com/contrib-badge/C63314D6Z.svg)](https://www.0crat.com/contrib/C63314D6Z)

[![EO principles respected here](https://www.elegantobjects.org/badge.svg)](https://www.elegantobjects.org)
[![Managed by Zerocracy](https://www.0crat.com/badge/C63314D6Z.svg)](https://www.0crat.com/p/C63314D6Z)
[![DevOps By Rultor.com](http://www.rultor.com/b/yegor256/eo)](http://www.rultor.com/p/yegor256/eo)
[![We recommend IntelliJ IDEA](https://www.elegantobjects.org/intellij-idea.svg)](https://www.jetbrains.com/idea/)

[![Build Status](https://travis-ci.org/yegor256/eo.svg?branch=master)](https://travis-ci.org/yegor256/eo)
[![PDD status](http://www.0pdd.com/svg?name=yegor256/eo)](http://www.0pdd.com/p?name=yegor256/eo)
[![Maintainability](https://api.codeclimate.com/v1/badges/e4f7ed144919f7f0d58c/maintainability)](https://codeclimate.com/github/yegor256/eo/maintainability)
[![Hits-of-Code](https://hitsofcode.com/github/yegor256/eo)](https://hitsofcode.com/view/github/yegor256/eo)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/yegor256/eo/blob/master/LICENSE.txt)
![Lines of code](https://img.shields.io/tokei/lines/github/yegor256/eo)
[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-compiler.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-compiler)

**EO** (stands for [Elegant Objects](http://www.yegor256.com/elegant-objects.html) or
ISO 639-1 code of [Esperanto](https://en.wikipedia.org/wiki/Esperanto))
is an object-oriented programming language. It's still a prototype.
It's the future of OOP. Please [contribute](https://github.com/yegor256/eo#how-to-contribute)!
By the way, we're aware of popular semi-OOP languages and we don't think
they are good enough, including
[Java](https://en.wikipedia.org/wiki/Java_%28programming_language%29),
[Ruby](https://en.wikipedia.org/wiki/Ruby_%28programming_language%29),
[C++](https://en.wikipedia.org/wiki/C%2B%2B),
[Smalltalk](https://en.wikipedia.org/wiki/Smalltalk),
[Python](https://en.wikipedia.org/wiki/Python_%28programming_language%29),
[PHP](https://en.wikipedia.org/wiki/PHP),
[C#](https://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29):
all of them have something we don't tolerate.

EO is not planning to become a mainstream language&mdash;this is not what
we want. Our main goal is to prove to ourselves that true object-oriented
programming is practically possible. Not just in books and abstract
examples, but in real code that works. That's why EO is being created&mdash;to
put all that "crazy" pure object-oriented ideas into practice and
see whether they can work. It's an experiment, a prototype, a proof-of-concept.

If you want to contribute, please join our
[Telegram chat](https://t.me/elegantobjects) first.

Our Twitter tag is [#eolang](https://twitter.com/search?q=%23eolang).

These things we **don't tolerate**:

  * static/class methods or attributes ([why?](http://www.yegor256.com/2014/05/05/oop-alternative-to-utility-classes.html))
  * classes ([why?](http://www.yegor256.com/2016/09/20/oop-without-classes.html))
  * implementation inheritance ([why?](http://www.yegor256.com/2016/09/13/inheritance-is-procedural.html))
  * mutability ([why?](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html))
  * NULL ([why?](http://www.yegor256.com/2014/05/13/why-null-is-bad.html))
  * global variables/procedures
  * reflection
  * type casting ([why?](http://www.yegor256.com/2015/04/02/class-casting-is-anti-pattern.html))
  * scalar types and data primitives
  * annotations ([why?](http://www.yegor256.com/2016/04/12/java-annotations-are-evil.html))
  * unchecked exceptions ([why?](http://www.yegor256.com/2015/07/28/checked-vs-unchecked-exceptions.html))
  * operators
  * flow control statements (`for`, `while`, `if`, etc)
  * DSL and [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) ([why?](https://github.com/yegor256/eo/issues/51))

We want EO to be compilable to Java. We want to stay as close to Java and JVM
as possible, mostly in order to re-use the eco-system and libraries
already available.

We also want to have an ability to compile it to any other language, like
Python, C/C++, Ruby, C#, etc. In other words, EO must be platform
independent.

## Quick Start

Here is a simple program that asks for a year and tells you
whether it's leap or not:

```
+alias stdout org.eolang.io.stdout
+alias stdin org.eolang.io.stdin
+alias scanner org.eolang.txt.scanner

[args] > main
  [y] > leap
    or
      and
        eq (mod y 4) 0
        not (eq (mod y 100) 0)
      eq (mod y 400) 0
  and
    stdout "Enter a year:"
    stdout
      concat
        (scanner stdin).nextInt > year!
        " is a leap year?"
        leap year:y
```

More examples are [here](https://github.com/yegor256/eo/tree/master/eo-maven-plugin/src/it).

## How to contribute

Fork repository, make changes, send us a pull request. We will review your changes and apply them to the master branch shortly, provided they don't violate our quality standards. To avoid frustration, before sending us your pull request please run full Maven build:

```bash
$ mvn clean install -Pqulice
```

## Troubleshooting

If after clean import from Maven, IDEA doesn't compile the project
and shows errors about ProgramLexer is undefined:

- Open Maven Projects window
- Select eo-compiler
- Right button
- Generate Sources and Update Folders
