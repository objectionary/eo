<!-- markdownlint-disable MD013 MD033 MD041 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

# eo-inference

Works out, for every object in an EO program, which object it was copied from.

EO has no types. It has objects, and every object is a copy of some other one,
which is a copy of another, and so on until the chain arrives at a formation
written in the source. That formation is the answer, and its FQN is the whole
of what this module means by a type:

```eo
[as-bytes] > number      # Φ.number.as-bytes is a Φ.bytes
  [x] > plus             # Φ.number.plus.x is a Φ.number
  [x] > minus
    $.^.plus ($.x.times -1) > @      # Φ.number.minus.@ is a Φ.number
```

Nothing else counts as an answer. `Φ.number.plus.x` is not "a number-ish
thing", it is `Φ.number`, and if we cannot say which formation it is we say so
rather than dressing up a guess.

The goal runs after `pre-inference` and writes three XML tables into
`target/eo/6-inference`. Nothing else in the compiler reads them yet.

## What it prints

```text
46896 objects: 81.0% named, 17.9% rooted at a void, 1.2% nothing known; depth 67.2%
```

Read that as: this program has 46,896 objects in it, we can name the formation
of four out of five, and one in eighty we cannot say a thing about.

The denominator is every object of the program, and it is never trimmed. The
atoms whose body is written in Java count against us, since this module cannot
read Java and so cannot say what they come back with unless the source says so
in as many words. A share that leaves out the hard cases is a share of the
easy ones.

### named

Coverage. The share whose answer is a formation of the program — a real FQN
that a reader can go and look at. A datum counts too: the bytes of `01-` are
the ground everything else stands on, and asking what more there is to know
about them is asking nothing. So does a termination.

Whether the formation still has voids free does not matter here. Knowing that
something is a `Φ.number.plus` is knowing which object it is, even before
knowing what went into its `x`.

### rooted at a void

The share we can only describe by pointing at somebody else's void. We know
that `Φ.inc.x.next` is the `next` of whatever fills `x`, and that is true of
every caller and concrete for none.

This is the honest middle. It is not coverage, because no formation is named;
it is not nothing, because the shape of the answer is there and one more fact
would finish it. Counting it as either would be a lie in one direction or the
other, so it gets a band of its own — and it is the band worth attacking,
being fifteen times the size of the one below it.

### nothing known

The share we say nothing about. Rare, and mostly the atoms whose body is Java.

### depth

A fourth number, of a different kind, which is why it sits behind a semicolon.
Each object stands on one of five rungs — nothing at all; a name rooted at a
void; a formation with voids still free; a formation with nothing left free;
nothing left to find out — and depth is the mean rung out of the highest one,
as a percentage.

It is an average of an ordinal scale, so it means nothing concrete: 67.2% is
not two thirds of anything. It is kept because it moves when a rule gets
sharper without moving an object from one band into another, which makes it a
finer instrument than the three shares for telling whether a change helped. It
must never be read as coverage.

Run with debug logging on to see the rungs themselves, which is the only way
to read any of these honestly — a share is a number to game, and writing an
empty row for every object would leave all four exactly where they are:

```text
   548  nothing at all
  8374  a name rooted at a void
  5953  a formation, voids still free
 22293  a formation, nothing left free
  9728  nothing left to find out
```

## What it draws

The three numbers say how much of a program we understand without saying which
part. A page per source file says both, with the author's own source on it and
a mark on every object: green where we can name the formation, amber where the
answer is somebody else's void, red where there is nothing. Hovering over a
mark says what the tables hold about it, and an amber one names what the
program was seen putting into the void besides.

The pages of eo-runtime are published at
[www.eolang.org/inference](https://www.eolang.org/inference/), rebuilt on every
tag, so looking at them needs nothing installed.

Drawing them is a goal of its own, `inference-report`, since the tables are
what the compiler needs and the pages are for a person: a build that wants
them asks for the goal, one that does not never runs it. The pom of eo-runtime
asks for it, so this is the shortest way to the pages of a working copy:

```bash
mvn -pl eo-runtime process-sources
open eo-runtime/target/site/inference/index.html
```

They land in the `target/site/inference/` of the module they describe, beside
the coverage report and every other generated page a person opens. They are
not written into `target/eo/`, which is the compiler's scratch space, however
much the tables they are made from live there.

## How it works

Every rule is a `Clue`: it reads the program and writes down one kind of fact.
No clue decides anything, and none of them can fail, so they compose in any
order:

```java
new Witnessed(new Demanded(new Resolved(new Clues())))
```

`Clues` is the first pass and fills the three tables from the source text
alone, writing a row for an object because it is *there* rather than because
something reaches it. That matters: eo-runtime is a library with no entry
point, so a checker that starts from a root and follows what runs would report
a clean bill of health for code it never opened.

| Table | Holds |
| --- | --- |
| `provides.xml` | What an object certainly has, read off the formation: its attributes, which of them are void, what it delegates to, what an atom comes back with. |
| `needs.xml` | What an object must have, judging by how it is used. `x.foo` means `x` needs a `foo`, whatever `x` turns out to be. |
| `links.xml` | Which object is a copy of which, and what every application puts into the voids of what it copies. |

The passes after it read those tables and write them again, each answering one
more question:

| Pass | Answers |
| --- | --- |
| `Resolved` | What every dispatch turns out to be. `a.b.c` is walked one hop at a time, each hop asked of the type the last one arrived at, looking behind a delegation and into a package where it has to. |
| `Demanded` | What a void will have to offer, gathered from every name ever asked of it, and what it will have to take, gathered from every call ever made on it. A contract: a caller that fills it owes these attributes, and the voids of what it fills with have to take these arguments. |
| `Witnessed` | What the program is actually seen to put into a void. Evidence, never a contract — the callers a program happens to have today do not oblige the one written tomorrow, and a void filled with a `Φ.number` everywhere is still a void. Nothing may work out a type from it. |

`Depth` then walks the finished tables and puts every object on its rung.

## How the behaviour is described

By packs, in
`eo-maven-plugin/src/test/resources/org/eolang/maven/inference-packs`. One pack
per behaviour: a small EO program of one or more files, and the XPaths its
tables must satisfy afterwards. A rule is described by the program it reads,
never by XMIR written out by hand.

```yaml
eo:
  app.eo: |
    [] > app
      inc oak > @
  inc.eo: |
    [x] > inc
      x > @
  oak.eo: |
    [] > oak
provides:
  - "//type[@id='Φ.inc']/attr[@void='true']/witnessed/ref[@loc='Φ.oak']"
```

Three tables is the whole set. A new fact becomes a column of one of them or a
child element inside a row — never a fourth document.

## What is not here

Checking. Judging whether a program is wrong lived beside these rules and was
taken out again in #6661, because it reported nothing: a verdict needs the
object that misses an attribute to have been seen whole, and almost none of
them have been. It comes back when every object can be given a type, rather
than only those a call site happens to reach.
