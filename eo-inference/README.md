<!-- markdownlint-disable MD013 MD033 MD041 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

# eo-inference

Reads what an EO program says about its own objects and writes it down in
`facts.xml`.

EO has no types. It has objects, and every object is a copy of some other one,
which is a copy of another, and so on until the chain arrives at a formation
written in the source. The engine that follows those chains, copy by copy, is
being ported here from Python (#9650). Until it lands, this module writes the
facts the engine reads, and draws the report of the `answers.xml` it writes.

## What it writes

The `inference` goal of `eo-maven-plugin` runs after `parse` and writes one
document into `target/eo/NN-inference`, where `seqdir` gives the directory its
number. It has four parts:

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
carries the EO sources of a small program, several files where it can, the
`facts.xml` written of it, and the `answers.xml` the engine gives on those
facts. `FactsTest` parses the sources the way Maven does and checks that the
facts come out the same, byte for byte.

The expected text of both was written by the Python engine the port is made
from, with its own copy of these classes, so a pack that fails here shows
where the Java and the Python sides part.

## What is not here

Checking. Judging whether a program is wrong lived beside these rules and was
taken out again in #6661, because it reported nothing: a verdict needs the
object that misses an attribute to have been seen whole, and almost none of
them have been. It comes back when every object can be given a type, rather
than only those a call site happens to reach.
