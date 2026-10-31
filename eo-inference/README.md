<!-- markdownlint-disable MD013 MD033 MD041 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

# eo-inference

Reads what an EO program says about its own objects and writes it down in
`facts.xml`, then works out from it which objects each place of the program
can be when Java runs it, writes that down in `answers.xml`, and draws a
report of those answers over the source.

EO has no types. It has objects, and every object is a copy of some other one,
which is a copy of another, and so on until the chain arrives at a formation
written in the source. The engine follows those chains copy by copy: a copy is
one use of a formation, with its own values in its voids, and every place is
answered once per copy of the formation around it.

## What it writes

The `inference` goal of `eo-maven-plugin` runs after `parse` and writes two
documents into `target/eo/NN-inference`, where `seqdir` gives the directory its
number. The first, `facts.xml`, has four parts:

- `objects`: every formation with its attributes. For each void it says
  whether a type is declared (`holds`, from `? > code /Q.number`) and
  whether an atom calls it back (`args`, from `? > scope /{Q.chunk}`). For an
  atom it says what it returns.
- `reads`: every name read, and what it is read off.
- `heads`: what each chain starts from: `Φ`, the formation around a bare
  `ξ`, a datum or a termination.
- `calls`: each call and its arguments in order.

Every object is named by its locator, a path from `Φ` down: `φ` is the `@`,
`ρ` is `^`, and `α0`, `α1` are the first and the second argument of a call.

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

The engine knows three lists of names of the program, which are parameters of
the goal with no defaults, and the `pom.xml` of eo-runtime sets them:
`inferenceCanon` (the kinds that are one shared copy each, `Φ.bytes`,
`Φ.number` and `Φ.string`), `inferenceRoots` (the packages of list code,
`Φ.tuple`, `Φ.map` and `Φ.set`) and `inferenceScalars` (the packages whose
objects give back new data). A list left unset is empty.

Before the facts are read, each file is made ready by `Prepared`: every chain
of names is split into one read per name, the first one too, so `x.next.foo`
becomes `.foo` read off `.next`, read off `.x`, read off a bare `ξ`, and the
locators are set again on the objects the split made. The prepared files are
kept in `target/eo/NN-pre-inference`, since every row of `facts.xml` points
into them.

## The report

The `inference-report` goal reads the prepared files and the `answers.xml` in
`target/eo/NN-inference`, and writes a page per source file into
`target/site/inference`, with an index. A page shows the source with a mark
on every object, coloured by one of ten bands of what the engine answers
there: green for one name and nothing else, then names with gaps, two names,
mixed names, lost track, voids an atom fills, voids nobody fills, only an
error, not reached, and red where a read fails. The goal logs how many
objects fall in each band.

`Report` draws the same bytes as `report.py`, the Python report it is ported
from, with the same `page.xsl` and `index.xsl`. `ReportTest` compares the two
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
