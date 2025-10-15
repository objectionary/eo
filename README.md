# Pure Object-Oriented Language, Experimental

[![EO principles respected here](https://www.elegantobjects.org/badge.svg)](https://www.elegantobjects.org)
[![DevOps By Rultor.com](https://www.rultor.com/b/objectionary/eo)](https://www.rultor.com/p/objectionary/eo)
[![We recommend IntelliJ IDEA](https://www.elegantobjects.org/intellij-idea.svg)](https://www.jetbrains.com/idea/)

[![mvn-linux](https://github.com/objectionary/eo/actions/workflows/mvn.yml/badge.svg)](https://github.com/objectionary/eo/actions/workflows/mvn.yml)
[![PDD status](https://www.0pdd.com/svg?name=objectionary/eo)](https://www.0pdd.com/p?name=objectionary/eo)
[![Maintainability](https://api.codeclimate.com/v1/badges/eaede7d027b1d9411a76/maintainability)](https://codeclimate.com/github/objectionary/eo/maintainability)
[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-parent.svg)](https://maven-badges.herokuapp.com/maven-central/org.eolang/eo-parent)
[![codecov](https://codecov.io/gh/objectionary/eo/branch/master/graph/badge.svg)](https://codecov.io/gh/objectionary/eo)
![Lines-of-Code](https://raw.githubusercontent.com/objectionary/eo/gh-pages/loc-badge.svg)
[![Hits-of-Code](https://hitsofcode.com/github/objectionary/eo)](https://hitsofcode.com/view/github/objectionary/eo)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/objectionary/eo/blob/master/LICENSE.txt)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fcqfn%2Feo.svg?type=shield)](https://app.fossa.com/reports/0ebb3149-4934-4565-bf6f-6fa41aed3b49)
[![Codacy Badge](https://app.codacy.com/project/badge/Grade/22dac7549c384692b79e02107de1d3c3)](https://www.codacy.com/gh/objectionary/eo/dashboard)
[![Known Vulnerabilities](https://snyk.io/test/github/objectionary/eo/badge.svg)](https://snyk.io/test/github/objectionary/eo)
[![Code Smells](https://sonarcloud.io/api/project_badges/measure?project=com.objectionary%3Aeo&metric=code_smells)](https://sonarcloud.io/summary/new_code?id=com.objectionary%3Aeo)

**EO** (stands for [Elegant Objects][book] or ISO 639-1 code of [Esperanto])
is an object-oriented programming language based on [𝜑-calculus].
We're aware of popular semi-OOP languages and we don't think
  they are good enough, including: [Java], [Ruby], [C++],
  [Smalltalk], [Python], [PHP], [C#].
All of them have something **we don't tolerate**:

* types ([why?](https://www.yegor256.com/2020/11/10/typing-without-types.html))
* static/class methods or attributes
  ([why?](http://www.yegor256.com/2014/05/05/oop-alternative-to-utility-classes.html))
* classes ([why?](http://www.yegor256.com/2016/09/20/oop-without-classes.html))
* implementation inheritance
  ([why?](http://www.yegor256.com/2016/09/13/inheritance-is-procedural.html))
* mutability
  ([why?](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html)
  and
  [why not?](https://www.yegor256.com/2016/09/07/gradients-of-immutability.html))
* NULL ([why?](http://www.yegor256.com/2014/05/13/why-null-is-bad.html))
* global scope
  ([why?](https://www.yegor256.com/2018/07/03/global-variables.html))
* type casting
  ([why?](http://www.yegor256.com/2015/04/02/class-casting-is-anti-pattern.html))
* reflection
  ([why?](https://www.yegor256.com/2022/06/05/reflection-means-hidden-coupling.html))
* scalar types and data primitives
* annotations
  ([why?](http://www.yegor256.com/2016/04/12/java-annotations-are-evil.html))
* operators
* traits and mixins
  ([why?](https://www.yegor256.com/2017/03/07/traits-and-mixins.html))
* flow control statements (`for`, `while`, `if`, etc)

## Quick Start

First, install [Java SE] and [npm].

Then, install [eoc]:

```bash
npm install -g eolang@0.33.3
```

Then, start with a simple EO program in the `app.eo` file:

```eo
# Just prints hello.
[args] > app
  QQ.io.stdout > @
    "Hello, world!\n"
```

Compile it like this (may take up to a minute or so):

```bash
eoc --easy link
```

Then, run it:

```bash
eoc --easy --alone dataize app
```

You should see "Hello, world!" printed.

## Simple Tutorial

In the example above, we create a new [abstract object][abstract objects]
named `app`, which has a single attribute named `@`. The object
attached to the attribute `@` is a copy of the object `stdout` with
a single argument `"Hello, world!"`. The object
`stdout` is also [abstract][abstract objects].
It can't be used directly, a copy of it has to be created,
with a few required arguments provided.
This is how a copy of the object `stdout` is made:

```text
QQ.io.stdout
  "Hello, world!\n"
```

The indentation in EO is important, just like in Python.
There must be two spaces
in front of the line in order to go to the deeper level of nesting.
This code can also be written in a "horizontal" notation:

```text
QQ.io.stdout "Hello, world!"
```

Moreover, it's possible to use brackets in order to group arguments and avoid
ambiguity. For example, instead of using a plain string `"Hello, world!"`
we may want to create a copy of the object `stdout` with a more complex
argument: a copy of the object `sprintf`:

```eo
# Says hello to Jeff.
[] > app
  QQ.io.stdout > @
    QQ.txt.sprintf
      "Hello, %s!"
      * "Jeffrey"
```

Here, the object `sprintf` is also [abstract][abstract objects].
It is being copied with two arguments: `"Hello, %s!"` and `"Jeffrey"`.
This program can be written using horizontal notation:

```eo
+alias org.eolang.io.stdout
+alias org.eolang.txt.sprintf

# Also says hello to Jeff.
[] > app
  stdout (sprintf "Hello, %s!" (* "Jeffrey")) > @
```

The special attribute `@` denotes an object that is being
[decorated][composable decorators].
In this example, the object `app` decorates the copy of the
object `stdout` and through this starts to behave like
the object `stdout`: all attributes of `stdout` become the
attributes of the `app`. The object `app` may have its own
attributes. For example, it's possible to define a new abstract object
inside `app` and use it to build the output string:

```eo
# Says hello to Jeff.
[] > app
  QQ.io.stdout (msg "Jeffrey") > @
  [name] > msg
    QQ.txt.sprintf "Hello, %s!" (* name) > @
```

Now, the object `app` has two "bound" attributes: `@` and `msg`. The attribute
`msg` has an abstract object attached to it, with a single "free" attribute
`name`.

This is how you iterate:

```eo
[args] > app
  malloc.for > @
    0
    [x] >>
      seq * > @
        x.put 2
        while
          x.as-number.lt 6 > [i] >>
          [i] >>
            seq * > @
              QQ.io.stdout
                QQ.txt.sprintf *1
                  "%d x %1$d = %d\n"
                  x
                  x.as-number.times x
              x.put
                x.as-number.plus 1
        true
```

This code will print this:

```text
2 x 2 = 4
3 x 3 = 9
4 x 4 = 16
5 x 5 = 25
```

Got the idea?

## Backus-Naur Form

This is our [EBNF] of EO language:

![Grammar][EO-Grammar]

The PNG image was
[auto-generated](https://github.com/objectionary/eo/actions/workflows/ebnf.yml).
It's better to use [ebnf/Eo.svg](https://www.eolang.org/ebnf/Eo.svg).

## What's Next?

Join [our Telegram group](https://t.me/eolang_org).

Watch [video](https://www.youtube.com/watch?v=QaKIw1Bh3Oc) about EOLANG basics.

Read [our blog], especially the section with
  [recently published papers][papers].

Learn [XMIR], a dialect of XML, which we use to represent EO program:
  [XSD] and [spec][XMIR HTML].

See the full collection of canonical objects: [objectionary][home].

Read more about integration [with Maven][eo-maven-plugin].

## Benchmark

This is how many milliseconds were spent on different XSL stylesheets
  during the execution of `mvn install` of the `eo-runtime` module:

<!-- benchmark_begin -->

```text
to-java.xsl              78929  41.08%
classes.xsl              60946  31.72%
set-locators.xsl         14251  7.42%
set-original-names.xsl   9729   5.06%
attrs.xsl                7444   3.87%
data.xsl                 6765   3.52%
package.xsl              4721   2.46%
anonymous-to-nested.xsl  4702   2.45%
tests.xsl                4639   2.41%
```

The results were calculated in [this GHA job][benchmark-gha]
on 2025-08-21 at 17:15,
on Linux with 4 CPUs.
The total is 192126 milliseconds.
We show only the first 16 most expensive XSL stylesheets.

<!-- benchmark_end -->

You can run this benchmark locally with the following commands.
First, to generate the `measures.csv` file:

```shell
mvn clean install --errors --batch-mode -Deo.xslMeasuresFile=measures.csv
```

Then, to generate the report:

```shell
awk -F ',' '{ a[$1]+=$2; s+=$2; } END { for (k in a) \
 printf("%s.xsl\t%d\t%0.2f%%\n", k, a[k], 100 * a[k]/s)}' \
 eo-runtime/measures.csv | sort -g -k 2 | tail -16 | column -t | head "-16"
```

## How to Contribute

Fork repository, make changes, then send us a [pull request][guidelines].
We will review your changes and apply them to the `master` branch shortly,
  provided they don't violate our quality standards.
To avoid frustration, before sending us your pull request
  please run full [Maven] build:

```bash
mvn clean install -Pqulice
```

You will need [Maven] 3.3+ and [Java] 11+ installed.
Also, if you have [xcop] installed, make sure it is version `0.8.0`+.

## Contributors

[![contributors](https://contributors-img.web.app/image?repo=objectionary/eo)](https://github.com/objectionary/eo/graphs/contributors)

## Special thanks

We are using the [YourKit Java Profiler]
to enhance the performance of EO components:

[![YourKit](https://www.yourkit.com/images/yklogo.png)](https://www.yourkit.com)

[abstract objects]: https://www.yegor256.com/2020/12/01/abstract-objects.html
[benchmark-gha]: https://github.com/objectionary/eo/actions/runs/17133812805
[C#]: https://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29
[C++]: https://en.wikipedia.org/wiki/C%2B%2B
[composable decorators]: https://www.yegor256.com/2015/02/26/composable-decorators.html
[EBNF]: https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form
[eo-maven-plugin]: https://github.com/objectionary/eo/tree/master/eo-maven-plugin
[eoc]: https://github.com/objectionary/eoc
[guidelines]: https://www.yegor256.com/2014/04/15/github-guidelines.html
[home]: https://github.com/objectionary/home
[Java SE]: https://www.oracle.com/java/technologies/downloads/
[Java]: https://en.wikipedia.org/wiki/Java_%28programming_language%29
[Maven]: https://maven.apache.org
[npm]: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
[our blog]: https://news.eolang.org
[papers]: https://news.eolang.org/papers.html
[PHP]: https://en.wikipedia.org/wiki/PHP
[Python]: https://en.wikipedia.org/wiki/Python_%28programming_language%29
[Ruby]: https://en.wikipedia.org/wiki/Ruby_%28programming_language%29
[Smalltalk]: https://en.wikipedia.org/wiki/Smalltalk
[xcop]: https://github.com/yegor256/xcop
[XMIR HTML]: https://www.eolang.org/XMIR.html
[XMIR]: https://news.eolang.org/2022-11-25-xmir-guide.html
[XSD]: https://www.eolang.org/XMIR.xsd
[YourKit Java Profiler]: https://www.yourkit.com/java/profiler
[book]: http://www.yegor256.com/elegant-objects.html
[𝜑-calculus]: https://arxiv.org/abs/2111.13384
[Esperanto]: https://en.wikipedia.org/wiki/Esperanto
[EO-Grammar]: https://raw.githubusercontent.com/objectionary/eo/gh-pages/ebnf/Eo.png
