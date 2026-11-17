<!-- markdownlint-disable MD013 MD033 MD041 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

# eo-inference

Full report is [available in HTML](https://www.eolang.org/inference/index.html).

Reads what an EO program says about its own objects and writes it down in
`facts.xml`, then works out from it which objects each place of the program
can be when Java runs it, writes that down in `answers.xml`, and draws a
report of those answers over the source.

EO has no types. It has objects, and every object is a copy of some other one,
which is a copy of another, and so on until the chain arrives at a formation
written in the source. The engine follows those chains copy by copy: a copy is
one use of a formation, with its own values in its voids, and every place is
answered once per copy of the formation around it. Code that nobody calls is
answered in a study copy, whose voids stay empty. In the end a study copy
counts only if its formation never got a copy from a call or a read and the
copy around it counts, while a copy from a call or a read counts wherever the
call or the read was. A call handed only unknowns waits until nothing else
grows, so that a real value can still come first, and then makes its copy
with those unknowns in its voids, each of its own kind. So a void of code
nobody calls stays that unknown down every call it goes through, and a read
of it never gives T. What Java puts into an object an atom returns comes from
the type of the atom. With brackets, the type gives each void its own type in
order, as `Φ.posix.status-return(Φ.number,Φ.string)` does, and a void the
brackets leave out holds the unknown only Java fills. With no brackets, as in
`Φ.bool`, each void holds what EO code puts into that void of that object:
the fork of a bool Java makes holds the formations `true` and `false` put
there. The engine reads these from `facts.xml` before the walk, so the answer
does not depend on the order of files or objects. A void that no EO code
fills holds the unknown only Java fills, and so does a void where some code
puts a thing only the walk could answer, next to whatever else goes in.

## What it writes

The `inference` goal of `eo-maven-plugin` runs after `parse` and writes two
documents into `target/eo/NN-inference`, where `seqdir` gives the directory its
number. The first, `facts.xml`, has four parts:

- `objects`: every formation with its attributes. For each void it says
  whether a type is declared (`holds`, from `? > code /Q.number`) and
  whether an atom calls it back (`args`, from `? > scope /{Q.chunk}`). For an
  atom it says what it returns, a type tree as the parser writes it, such as
  `Φ.posix.status-return(Φ.number,Φ.string)` or `Φ.bool`.
- `reads`: every name read, and what it is read off.
- `heads`: what each chain starts from: `Φ`, the formation around a bare
  `ξ`, a datum or a termination.
- `calls`: each call and its arguments in order.

Every object is named by its locator, a path from `Φ` down: `φ` is the `@`,
`ρ` is `^`, and `α0`, `α1` are the first and the second argument of a call.
The `^` of a named formation is the formation around it, or `Φ` at the top.
A formation with no name, written as an argument, takes as its `^` the object
it goes into, an atom too, and one that goes into no object has a `^` nobody
fills.

The second, `answers.xml`, is what the engine (`Walk`) works out from
`facts.xml` alone. For every place it has the sets of objects the copies of
that place can be, with the number of copies that say each set, what the
copies of each formation reduce to, and, for an argument, the one void it
lands in:

```xml
<o loc="Φ.box.item" copies="2">
  <set copies="1">
    <is loc="Φ.number"/>
  </set>
  <set copies="1">
    <is loc="Φ.string"/>
  </set>
</o>
```

A copy is named by its formation, but a copy of a decorator is named by what
its `@` gives. Two rows are the exception and name the formation of each
copy in them. One is the row of a `^`. The other is the row of the object a
name is read off, for each copy that has that name itself, as a void or a
bound attribute of its formation: `accept` in `socket.eo` reads `scope` and
`cant-accept` off itself, so the `$` in front of each names `accept`. A copy
that lacks the name hands the read on to its `@`, and there the row names
what that `@` gives, as anywhere else. When the copies a row names reduce to
something else, a `<reduced>` next to the names says what:

```xml
<o loc="Φ.app.φ.α0.ρ" copies="1">
  <is loc="Φ.foo"/>
  <reduced copies="1">
    <is loc="Φ.number"/>
  </reduced>
</o>
```

The row of a formation has a `study` attribute: how many of its copies that
count are study copies or copies as written that a stop kept. A formation
with none has no `study`. Nobody calls `read` in `socket.eo`, so its one
copy is a study copy:

```xml
<o loc="Φ.socket.as-input.read" copies="1" study="1">
  <is loc="Φ.socket.as-input.read"/>
  <reduced copies="1">
    <is loc="Φ.socket.as-input.read"/>
  </reduced>
</o>
```

The engine knows three lists of names of the program, which are parameters of
the goal with no defaults, and the `pom.xml` of eo-runtime sets them:
`infer.canon` (the kinds that are one shared copy each, `Φ.bytes`,
`Φ.number` and `Φ.string`), `infer.roots` (the packages of list code,
`Φ.tuple`, `Φ.map` and `Φ.set`) and `infer.scalars` (the packages whose
objects give back new data). A list left unset is empty.

Before the facts are read, each file is made ready by `Prepared`: every chain
of names is split into one read per name, the first one too, so `x.next.foo`
becomes `.foo` read off `.next`, read off `.x`, read off a bare `ξ`, and the
locators are set again on the objects the split made. The prepared files are
kept in `target/eo/NN-pre-inference`, since every row of `facts.xml` points
into them.

## The report

The `render` goal reads the prepared files and the `answers.xml` in
`target/eo/NN-inference`, and writes a page per source file into
`target/site/inference`, with an index. A page shows the source with a mark
on every object, coloured by one of nine bands of what the engine answers
there: green for one name and nothing else, then names with gaps, two names,
mixed names, an unknown of each of four kinds, and only an error. The four
kinds are a read nothing ever answers, a void only Java fills, a void of an
object nobody calls, and a void nobody fills, and `answers.xml` writes them
as `<unknown kind="loop"/>`, `kind="java"`, `kind="study"` and
`kind="empty"`. A place with no name and unknowns of several kinds takes the
first of them in that order. The pop-up of the mark on the name of a
formation with a `study` ends with the line
`studied: nobody calls this object`, followed by `(1 of 3 copies)` when only
some of its copies are study copies. The marks of its attributes get no
such line, and the bands and the tally ignore it. The goal logs how many objects
fall in each band. An object that `answers.xml` says nothing of stops the
report with an error that names its locator.

`Report` draws the same bytes as the Python report it is ported from, with the
same `page.xsl` and `index.xsl`. `ReportTest` compares the two
on a whole run when given its files:

```bash
mvn test -pl eo-inference -Dtest=ReportTest \
  -Dreport.xmirs=pre -Dreport.answers=answers.xml -Dreport.html=html
```

## How the behaviour is described

By test packs in `src/test/resources/org/eolang/inference/packs`. Each one
carries the EO sources of a small program, several files where it can, a
comment at the top that says what the engine gives on it, and a list of
XPaths, each checking one claim of that comment. `AnswersTest` makes the
facts of the sources the way Maven does, runs the engine on them, and checks
the XPaths under `answers:` against the `answers.xml` it writes. A pack whose
rule is about the facts also lists XPaths under `facts:`, and `FactsTest`
checks them against the `facts.xml`; it skips the packs that have none.

## What is not here

Checking. Judging whether a program is wrong lived beside these rules and was
taken out again in #6661, because it reported nothing: a verdict needs the
object that misses an attribute to have been seen whole, and almost none of
them have been. It comes back when every object can be given a type, rather
than only those a call site happens to reach.
