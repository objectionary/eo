<img src="https://www.yegor256.com/images/books/elegant-objects/cactus.svg" height="100px" />

[![Donate via Zerocracy](https://www.0crat.com/contrib-badge/C63314D6Z.svg)](https://www.0crat.com/contrib/C63314D6Z)

[![EO principles respected here](https://www.elegantobjects.org/badge.svg)](https://www.elegantobjects.org)
[![Managed by Zerocracy](https://www.0crat.com/badge/C63314D6Z.svg)](https://www.0crat.com/p/C63314D6Z)
[![DevOps By Rultor.com](http://www.rultor.com/b/cqfn/eo)](http://www.rultor.com/p/cqfn/eo)
[![We recommend IntelliJ IDEA](https://www.elegantobjects.org/intellij-idea.svg)](https://www.jetbrains.com/idea/)

[![mvn](https://github.com/cqfn/eo/actions/workflows/mvn.yml/badge.svg)](https://github.com/cqfn/eo/actions/workflows/mvn.yml)
[![Build Status](https://travis-ci.org/cqfn/eo.svg?branch=master)](https://travis-ci.org/cqfn/eo)
[![PDD status](http://www.0pdd.com/svg?name=cqfn/eo)](http://www.0pdd.com/p?name=cqfn/eo)
[![Maintainability](https://api.codeclimate.com/v1/badges/e4f7ed144919f7f0d58c/maintainability)](https://codeclimate.com/github/cqfn/eo/maintainability)
[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-parent.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-parent)

[![codecov](https://codecov.io/gh/cqfn/eo/branch/master/graph/badge.svg)](https://codecov.io/gh/cqfn/eo)
[![Hits-of-Code](https://hitsofcode.com/github/cqfn/eo)](https://hitsofcode.com/view/github/cqfn/eo)
![Lines of code](https://img.shields.io/tokei/lines/github/cqfn/eo)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/cqfn/eo/blob/master/LICENSE.txt)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fcqfn%2Feo.svg?type=shield)](https://app.fossa.com/reports/0ebb3149-4934-4565-bf6f-6fa41aed3b49)

**EO** (stands for [Elegant Objects](http://www.yegor256.com/elegant-objects.html) or
ISO 639-1 code of [Esperanto](https://en.wikipedia.org/wiki/Esperanto))
is an object-oriented programming language. It's still a prototype.
It's the future of OOP. Please [contribute](https://github.com/cqfn/eo#how-to-contribute)!
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

EO is based on 𝜑-calculus (if you want to see its description, 
join this Telegram chat: [@polystat_org](https://t.me/polystat_org)).

If you want to contribute, please join our
[Telegram chat](https://t.me/elegantobjects) first.

Our Twitter tag is [#eolang](https://twitter.com/search?q=%23eolang).

These things we **don't tolerate**:

  * static/class methods or attributes ([why?](http://www.yegor256.com/2014/05/05/oop-alternative-to-utility-classes.html))
  * classes ([why?](http://www.yegor256.com/2016/09/20/oop-without-classes.html))
  * implementation inheritance ([why?](http://www.yegor256.com/2016/09/13/inheritance-is-procedural.html))
  * mutability ([why?](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html))
  * NULL ([why?](http://www.yegor256.com/2014/05/13/why-null-is-bad.html))
  * global scope ([why?](https://www.yegor256.com/2018/07/03/global-variables.html))
  * type casting ([why?](http://www.yegor256.com/2015/04/02/class-casting-is-anti-pattern.html))
  * reflection
  * scalar types and data primitives
  * annotations ([why?](http://www.yegor256.com/2016/04/12/java-annotations-are-evil.html))
  * unchecked exceptions ([why?](http://www.yegor256.com/2015/07/28/checked-vs-unchecked-exceptions.html))
  * operators
  * flow control statements (`for`, `while`, `if`, etc)
  * DSL and [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) ([why?](https://github.com/cqfn/eo/issues/51))

We want EO to be compilable to Java. We want to stay as close to Java and JVM
as possible, mostly in order to re-use the eco-system and libraries
already available.

We also want to have an ability to compile it to any other language, like
Python, C/C++, Ruby, C#, etc. In other words, EO must be platform
independent.

## Friendly Products

There are products that use EO or 𝜑-calculus:

  * [polystat](https://github.com/polystat/polystat) is a static analyzer of EO programs
  * [eo2py](https://github.com/polystat/eo2py) is a translator of EO to Python

If you want yours to be in the list, submit a pull request.

## Quick Start

Here is a simple program that gets a year from command line and tells you
whether it's leap or not:

```
+alias org.eolang.io.stdout

[args...] > main
  [y] > leap
    or. > @
      and.
        eq. (mod. y 4) 0
        not. (eq. (mod. y 100) 0)
      eq. (mod. y 400) 0
  stdout > @
    sprintf
      "%d is %sa leap year!"
      (args.get 0).nextInt > year!
      if (leap year:y) "" "not "
```

In order to compile this program, put it into `src/main/eo/main.eo` and then
create a file `pom.xml` with this content (it's just a sample):

```xml
<project>
  [...]
  <build>
    <plugins>
      <plugin>
        <groupId>org.eolang</groupId>
        <artifactId>eo-maven-plugin</artifactId>
        <version><!-- check Maven Central --></version>
        <executions>
          <execution>
            <goals>
              <goal>parse</goal>
              <goal>optimize</goal>
              <goal>compile</goal>
            </goals>
          </execution>
        </executions>
      </plugin>
      <plugin>
        <groupId>org.codehaus.mojo</groupId>
        <artifactId>exec-maven-plugin</artifactId>
        <executions>
          <execution>
            <phase>test</phase>
            <goals>
              <goal>java</goal>
            </goals>
          </execution>
        </executions>
        <configuration>
          <mainClass>org.eolang.phi.Main</mainClass>
          <arguments>
            <argument>main</argument>
            <argument>2008</argument>
          </arguments>
        </configuration>
      </plugin>
    </plugins>
  </build>
  <dependencies>
    <dependency>
      <groupId>org.eolang</groupId>
      <artifactId>eo-runtime</artifactId>
      <version><!-- check Maven Central --></version>
    </dependency>
  </dependencies>
</project>
```

Then, you just run `mvn clean test` (you will need [Maven 3.3+](https://maven.apache.org/))
and the `.eo` file will be parsed to `.xml` files, transformed to `.java` files,
and then compiled to `.class` files. You can see them all in the `target` directory.
You will need Java 8+.

More examples are [here](https://github.com/cqfn/eo/tree/master/eo-maven-plugin/src/it).

## Tutorial

Let's start with a simple EO program:

```
+alias stdout org.eolang.io.stdout

[] > app
  stdout > @
    "Hello, world!"
```

Here we create a new [abstract object](https://www.yegor256.com/2020/12/01/abstract-objects.html)
named `app`, which has got a single attribute named `@`. The object attached to the attribute
`@` is a copy of the object `stdout` with a single argument `"Hello, world!"`. The object
`stdout` is also [abstract](https://www.yegor256.com/2020/12/01/abstract-objects.html).
It can't be used directly, a copy of it has to be created, with a few requirement arguments provided.
This is how a copy of the object `stdout` is made:

```
stdout
  "Hello, world!"
```

The indentation in EO is important, just like in Python. There have to be two spaces
in front of the line in order to go to the deeper level of nesting. This code can also be written
in a "horizontal" notation:

```
stdout "Hello, world!"
```

Moreover, it's possible to use brackets in order to group arguments and avoid
ambiguity. For example, instead of using a plain string `"Hello, world!"`
we may want to create a copy of the object `stdout` with a more complex
argument: a copy of the object `sprintf`:

```
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

[] > app
  stdout > @
    sprintf
      "Hello, %s!"
      "Jeffrey"
```

Here, the object `sprintf` is also [abstract](https://www.yegor256.com/2020/12/01/abstract-objects.html).
It is being copied with two arguments: `"Hello, %s!"` and `"Jeffrey"`. This program
can be written using horizontal notation:

```
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

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
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

[] > app
  stdout (msg "Jeffrey") > @
  [name] > msg
    sprintf "Hello, %s!" name > @
```

Now, the object `app` has two "bound" attributes: `@` and `msg`. The attribute
`msg` has an abstract object attached to it, with a single "free" attribute
`name`.

This is how you iterate:

```
+package sandbox
+alias stdout org.eolang.io.stdout
+alias sprintf org.eolang.txt.sprintf

[args...] > app
  memory > x
  seq > @
    x.write 2
    while.
      x.less 6
      [i]
        seq > @
          stdout
            sprintf "%dx%d = %d\n" x x (x.pow 2)
          x.write (x.add 1)

```

This code will print this:

```
2 x 2 = 4
3 x 3 = 9
4 x 4 = 16
5 x 5 = 25
```

Got the idea?

## How it Works?

The entire process of turning an `.eo` program into an executable
binary code constists of a few steps, which must be done
one after another:

  * **Parsing**.
    It's done by the `org.eolang.parser.Syntax` class in the `eo-parser` module. It takes
    the source code in a plain text format and parses into XML document,
    using [ANTLR4](https://www.antlr.org/) and [Xembly](https://www.xembly.org).
    The output of the parser you can find in the `target/eo/parse` directory.

  * **Optimization**.
    There are a number of [XSL transformations](https://en.wikipedia.org/wiki/XSLT)
    that need to be done
    with the XML document in order to make it ready for compilation.
    Each transformation has its own `.xsl` file in the `eo-parser` directory.
    The class `org.eolang.parser.Program` is responsible for making XSLT
    transformations and the entire list of them is stored in the
    `org.eolang.parser.Pack` class. Some of XLST files are sanity checks (or linters).
    The output of each transformation you can find in the `target/eo/optimize` directory.

  * **Compilation**.
    The class `org.eolang.maven.CompileMojo` in the `eo-maven-plugin` module is responsible
    for putting parsing and optimization steps together and then transforming
    the XML document into a collection of `.java` files. There are a number
    of transformations that do this, they all exist in `.xsl` files.
    The output of this step you can find in the `target/generated-sources` directory.

There is also a module called `eo-runtime`, which includes both `.eo` and `.java` code
for most popular and important objects that any of you will need in order
to write even a simple EO program. There are objects like `string`, `int`, `sprintf`,
`stdout`, and so on. By the way, you may want to contribute there by creating new objects.

## How to Contribute

Fork repository, make changes, send us a pull request.
We will review your changes and apply them to the `master` branch shortly,
provided they don't violate our quality standards. To avoid frustration,
before sending us your pull request please run full Maven build:

```bash
$ mvn clean install -Pqulice
```

You will need Maven 3.3+ and Java 8+.

