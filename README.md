<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

[![EO principles respected here](https://www.elegantobjects.org/badge.svg)](https://www.elegantobjects.org)
[![DevOps By Rultor.com](http://www.rultor.com/b/objectionary/eo)](http://www.rultor.com/p/objectionary/eo)
[![We recommend IntelliJ IDEA](https://www.elegantobjects.org/intellij-idea.svg)](https://www.jetbrains.com/idea/)

[![mvn-linux](https://github.com/objectionary/eo/actions/workflows/mvn.yml/badge.svg)](https://github.com/objectionary/eo/actions/workflows/mvn.yml)
[![PDD status](http://www.0pdd.com/svg?name=objectionary/eo)](http://www.0pdd.com/p?name=objectionary/eo)
[![Maintainability](https://api.codeclimate.com/v1/badges/b8b59692f3c8c973ac54/maintainability)](https://codeclimate.com/github/cqfn/eo/maintainability)
[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-parent.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-parent)
[![codecov](https://codecov.io/gh/objectionary/eo/branch/master/graph/badge.svg)](https://codecov.io/gh/objectionary/eo)
[![Hits-of-Code](https://hitsofcode.com/github/objectionary/eo)](https://hitsofcode.com/view/github/objectionary/eo)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/objectionary/eo/blob/master/LICENSE.txt)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fcqfn%2Feo.svg?type=shield)](https://app.fossa.com/reports/0ebb3149-4934-4565-bf6f-6fa41aed3b49)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/22dac7549c384692b79e02107de1d3c3)](https://www.codacy.com/gh/objectionary/eo/dashboard)
[![Known Vulnerabilities](https://snyk.io/test/github/objectionary/eo/badge.svg)](https://snyk.io/test/github/objectionary/eo)
[![Code Smells](https://sonarcloud.io/api/project_badges/measure?project=com.objectionary%3Aeo&metric=code_smells)](https://sonarcloud.io/summary/new_code?id=com.objectionary%3Aeo)

**EO** (stands for [Elegant Objects](http://www.yegor256.com/elegant-objects.html) or
ISO 639-1 code of [Esperanto](https://en.wikipedia.org/wiki/Esperanto))
is an object-oriented programming language based on
[𝜑-calculus](https://arxiv.org/abs/2111.13384).
We're aware of popular semi-OOP languages and we don't think
they are good enough, including:
[Java](https://en.wikipedia.org/wiki/Java_%28programming_language%29),
[Ruby](https://en.wikipedia.org/wiki/Ruby_%28programming_language%29),
[C++](https://en.wikipedia.org/wiki/C%2B%2B),
[Smalltalk](https://en.wikipedia.org/wiki/Smalltalk),
[Python](https://en.wikipedia.org/wiki/Python_%28programming_language%29),
[PHP](https://en.wikipedia.org/wiki/PHP),
[C#](https://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29).
All of them have something **we don't tolerate**:

  * types ([why?](https://www.yegor256.com/2020/11/10/typing-without-types.html))
  * static/class methods or attributes ([why?](http://www.yegor256.com/2014/05/05/oop-alternative-to-utility-classes.html))
  * classes ([why?](http://www.yegor256.com/2016/09/20/oop-without-classes.html))
  * implementation inheritance ([why?](http://www.yegor256.com/2016/09/13/inheritance-is-procedural.html))
  * mutability ([why?](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html) and [why not?](https://www.yegor256.com/2016/09/07/gradients-of-immutability.html))
  * NULL ([why?](http://www.yegor256.com/2014/05/13/why-null-is-bad.html))
  * global scope ([why?](https://www.yegor256.com/2018/07/03/global-variables.html))
  * type casting ([why?](http://www.yegor256.com/2015/04/02/class-casting-is-anti-pattern.html))
  * reflection ([why?](https://www.yegor256.com/2022/06/05/reflection-means-hidden-coupling.html))
  * scalar types and data primitives
  * annotations ([why?](http://www.yegor256.com/2016/04/12/java-annotations-are-evil.html))
  * operators
  * traits and mixins ([why?](https://www.yegor256.com/2017/03/07/traits-and-mixins.html))
  * flow control statements (`for`, `while`, `if`, etc)
  * [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) ([why?](https://github.com/objectionary/eo/issues/51))

## Quick Start

First, install [Java SE](https://www.oracle.com/java/technologies/downloads/),
[npm](https://docs.npmjs.com/downloading-and-installing-node-js-and-npm), 
[Rust with Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
and [eoc](https://github.com/objectionary/eoc).

Then, start with a simple EO program in `app.eo` file:

```
[args...] > app
  QQ.io.stdout > @
    "Hello, world!\n"
```

Compile it like this (may take a minute or so):

```
$ eoc link
```

Then, run it:

```
$ eoc --alone dataize app
```

You should see "Hello, world!" printed.

## Simple Tutorial

In the example above, we create a new [abstract object](https://www.yegor256.com/2020/12/01/abstract-objects.html)
named `app`, which has got a single attribute named `@`. The object attached to the attribute
`@` is a copy of the object `stdout` with a single argument `"Hello, world!"`. The object
`stdout` is also [abstract](https://www.yegor256.com/2020/12/01/abstract-objects.html).
It can't be used directly, a copy of it has to be created, with a few requirement arguments provided.
This is how a copy of the object `stdout` is made:

```
QQ.io.stdout
  "Hello, world!"
```

The indentation in EO is important, just like in Python. There have to be two spaces
in front of the line in order to go to the deeper level of nesting. This code can also be written
in a "horizontal" notation:

```
QQ.io.stdout "Hello, world!"
```

Moreover, it's possible to use brackets in order to group arguments and avoid
ambiguity. For example, instead of using a plain string `"Hello, world!"`
we may want to create a copy of the object `stdout` with a more complex
argument: a copy of the object `sprintf`:

```
[] > app
  QQ.io.stdout > @
    QQ.txt.sprintf
      "Hello, %s!"
      "Jeffrey"
```

Here, the object `sprintf` is also [abstract](https://www.yegor256.com/2020/12/01/abstract-objects.html).
It is being copied with two arguments: `"Hello, %s!"` and `"Jeffrey"`. This program
can be written using horizontal notation:

```
+alias org.eolang.io.stdout
+alias org.eolang.txt.sprintf

[] > app
  (stdout (sprintf "Hello, %s!" "Jeffrey")) > @
```

The special attribute `@` denotes an object that is being
[decorated](https://www.yegor256.com/2015/02/26/composable-decorators.html).
In this example, the object `app` decorates the copy of the
object `stdout` and through this starts to behave like
the object `stdout`: all attributes of `stdout` become the
attributes of the `app`. The object `app` may have its own
attributes. For example, it's possible to define a new abstract object
inside `app` and use it to build the output string:

```
[] > app
  QQ.io.stdout (msg "Jeffrey") > @
  [name] > msg
    QQ.txt.sprintf "Hello, %s!" name > @
```

Now, the object `app` has two "bound" attributes: `@` and `msg`. The attribute
`msg` has an abstract object attached to it, with a single "free" attribute
`name`.

This is how you iterate:

```
[args...] > app
  memory 0 > x
  seq > @
    *
      x.write 2
      while.
        x.lt 6
        [i]
          seq > @
            * 
              QQ.io.stdout
                QQ.txt.sprintf
                  "%d x %d = %d\n"
                  x
                  x
                  x.times x
              x.write
                x.plus 1
    TRUE
```

This code will print this:

```
2 x 2 = 4
3 x 3 = 9
4 x 4 = 16
5 x 5 = 25
```

Got the idea?

## Backus-Naur Form

This is our [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form), of EO language:

<img alt="ENBF of EO" src="https://www.eolang.org/ebnf/Eo.png" width="100%" />

This is the [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) of 𝜑-calculus:

<img alt="ENBF of 𝜑-calculus" src="https://www.eolang.org/ebnf/Phi.png" width="100%" />

The images were [auto-generated](https://github.com/objectionary/eo/actions/workflows/ebnf.yml). 
It's better to use [ebnf/Eo.svg](https://www.eolang.org/ebnf/Eo.svg) 
and [ebnf/Phi.svg](https://www.eolang.org/ebnf/Phi.svg).

## What's Next?

Join [our Telegram group](https://t.me/polystat_org).

Watch [video](https://www.youtube.com/watch?v=QaKIw1Bh3Oc) about EOLANG basics.

Read [our blog](https://news.eolang.org), especially the section with
[recently published papers](https://news.eolang.org/papers.html).

Learn [XMIR](https://news.eolang.org/2022-11-25-xmir-guide.html), a dialect of XML, which we use to represent EO program.

See the full collection of canonical objects: [objectionary](https://github.com/objectionary/home).

Take a look how we use EO as an Intermediary Representation (IR) in
[Polystat](https://www.polystat.org), a polyglot static analyzer.

Play with more examples [here](https://github.com/objectionary/sandbox).

Read about integration with Maven,
[here](https://github.com/objectionary/eo/tree/master/eo-maven-plugin).

## How to Contribute

Fork repository, make changes, then send us a [pull request](https://www.yegor256.com/2014/04/15/github-guidelines.html).
We will review your changes and apply them to the `master` branch shortly,
provided they don't violate our quality standards. To avoid frustration,
before sending us your pull request please run full Maven build:

```bash
$ mvn clean install -Pqulice
```

You will need [Maven 3.3+](https://maven.apache.org) and Java 8+ installed.

## Special thanks
We are using the [YourKit Java Profiler](https://www.yourkit.com/java/profiler)
to enhance the performance of EO components:

[![YourKit](https://www.yourkit.com/images/yklogo.png)](https://www.yourkit.com)
