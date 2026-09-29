<!-- markdownlint-disable MD013 MD033 MD041 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

# eo-inference

Works out, for every object in an EO program, which formation it behaves as.

EO has no types. It has objects, and every object is a copy of some other one,
which is a copy of another, and so on until the chain arrives at a formation
written in the source. That formation is where the answer is looked for, and an
FQN is the whole of what this module means by a type:

```eo
[@] > number             # Φ.number.φ is a Φ.bytes
  [b] > plus             # Φ.number.plus.b is a Φ.number
  [b] > minus
    $.^.plus ($.b.times -1) > @      # Φ.number.minus.@ is a Φ.number
```

Where the chain arrives is not always where it stops. A formation that binds
nothing the outside can read but its own `φ` has no behaviour of its own: it
hands on everything it can be asked, and its name says less about an object
than the name behind it. So `1.plus 2` is a `Φ.number` rather than a
`Φ.number.plus`, and the `as-bytes` of a `Φ.bytes` is a `Φ.bytes`. A formation
that does bind something of its own is where the walk ends, and if we cannot
say which formation it is we say so rather than dressing up a guess.

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
something is a `Φ.number` is knowing which object it is, even before knowing
what went into its `φ`.

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

### written down

The line scrolls past, so the same numbers also go into `target/eo/ladder.txt`,
beside the tables rather than among them, because they measure us and not the
program:

```text
46896 objects
81.0 named
17.9 rooted at a void
1.2 nothing known
67.2 depth
548 nothing at all
8374 a name rooted at a void
5953 a formation, voids still free
22293 a formation, nothing left free
9728 nothing left to find out
581 rooted at a void nobody fills
7790 rooted at a void the callers fill
3 rooted at a void only an atom fills
745 answered with a choice
```

One number a line, the value first and the name of it after, so that a shell
can read it with one `read` and know nothing about what any of it means. The
rungs go down with the shares and not instead of them, for the reason above.

The three under the rung of the voids go down with them for a reason of their
own. That rung is one number and three different situations, and only one of
the three is ours. A void nobody fills and a void only Java fills are as far
as anybody can go: the name is weak and it is true, and no amount of work will
make it say more. A void the callers of the program fill is a gap we left,
since the program says what goes in there and `Witnessed` wrote it down, so a
name still rooted at it means we did not use what we recorded. Added together
the three make a share that cannot get worse when we are wrong, which is the
one thing a measurement of ourselves must never do. They are bands rather than
rungs, so `Band` works them out and a page and a tally cannot disagree about
them, and they sum to the rung above.

The last line is neither a share nor a rung. A call on a void that holds a
picker hands back one of the arguments it was given, and where those agree on
nothing the row names all of them rather than none. That is a real answer, and
no rung can show it: the walk ended at the void either way, so an object told
it is either a `Φ.dial` or a `Φ.clock` is counted beside an object told
nothing. It goes last and on its own, because a share that started counting
arms would be a share nobody could compare against an older build.

A pull request that touches the rules, the parser, the plugin or the program
they are read from is built twice by `.github/workflows/ladder.yml` — once at
the branch and once at the commit it sits on — and every line that differs
between the two files is posted on it as a table. A branch that moved nothing
is told nothing.

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
new Witnessed(new Demanded(new Reduced(new Resolved(new Clues()))))
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
| `Reduced` | Which name a type goes by when it has no behaviour of its own. A formation whose only public attribute is its `φ` hands on everything it can be asked, so the name behind it is written on the row and every object that settled on it is reported as that instead. |
| `Demanded` | What a void will have to offer, gathered from every name ever asked of it, and what it will have to take, gathered from every call ever made on it. A contract: a caller that fills it owes these attributes, and the voids of what it fills with have to take these arguments. |
| `Witnessed` | What the program is seen to put into every void, gathered from every application that fills it, as the choice between the types put there. A build reads the program whole, library and all, so this is every caller there is, and rule 6 below reads it as the answer rather than as evidence of one. |
| `Named` | Which object a void is settled at, read off the census `Witnessed` gathered, where that census names one type the table has a row for; several members become a choice under #8982. It is written on the row of the void as `settled`, and on every link that stops at the void, marked `witnessed`. A link that stops at a void the source declared is told the `holds` instead, unmarked, so the read of a `^` names its owner. |

`Depth` then walks the finished tables and puts every object on its rung.

A void row carries two answers and they are not the same answer. `holds` is
what the source declared, in `? > code /Q.number`, true of every caller there
will ever be; `settled` is what `Named` read off the census, true of the callers
this program has, which are all of them. It is written only where the source
declared nothing, a reader after the type of a void reads it second, and it may
be a choice.

A link says where its answer came from. A `ref` or a `bind` that was reached
only through what the program was seen to put into a void carries
`witnessed="true"`: the call on a void that its one caller fills with a
`refused` is a copy of `refused` and fills its `message` because that caller
does. The mark is provenance and not doubt, since the callers of the program
are all the callers there are; it says which rows change when a caller does.
A pass renaming arguments after the voids they land in leaves such rows out
today, and #8982 decides whether it goes on doing so:

```xml
<type id="Φ.socket.connect.φ.α0">
  <ref loc="Φ.socket.refused" witnessed="true">
    <bind void="Φ.socket.refused.message" witnessed="true">
      <ref loc="Φ.socket.connect.φ.α0.α0"/>
    </bind>
  </ref>
</type>
```

A `ref` is marked where the passes, run once more with no void named after
its callers, do not arrive at it, and wherever `Named` wrote it from the
census; a `bind` is marked where only the relay into what a void holds put it
there.

### What a choice comes back as

`x.if a b` is never about `x`. The `if` of a boolean is a void, `true` fills
it with a formation that hands back its first argument and `false` with one
that hands back its second, so the call is one of its two arms, and which one
is not known. Every rule below is about an arm: what it is, and what the row
that reads the call may therefore claim. A program is everything compiled
together and there are no later callers, so a row asserts what every run of
the program does, and a rule that would need a caller who is not there is not
a rule of this module.

**Rule 1. An arm that is a picker's input is what the call passed.** The
`if` of a boolean holds a formation that gives back one of its inputs and
nothing of its own:

```eo
[if] > bool
  if > @

[^] > true
  bool > @
    [^ left right]
      left > @

[^] > false
  bool > @
    [^ left right]
      right > @

[] > app
  flag.if a b > answer
```

`answer` is `a` or `b`. Where both are a `Φ.file` the row says `Φ.file`;
where one is a `Φ.file` and the other a `Φ.string` it says the choice of the
two, written as a `union` inside the `ref` so that the row stays a pair
(#8744).

**Rule 2. An arm the formation computes itself is that body's own type,**
whatever went into the slots:

```eo
[] > odd
  [left right] > if
    42 > @

[] > app
  odd.if a b > answer
```

`answer` is a `Φ.number` whatever `a` and `b` are, since this `if` reads
neither. Where `flag` above may hold `true`, `false` or `odd`, every one of
them contributes an arm and the row is the choice of `a`, `b` and
`Φ.number`. A body nobody has settled is an unknown member of that choice,
never a guess.

**Rule 3. An arm that is the `^` of an attribute is exactly its owner.**
`made` is an attribute of `directory`, so only a `Φ.directory` ever sits in
its `^`:

```eo
[] > directory
  [^] > made
    ^.exists.if > @
      ^
      ^.created

[] > wrapper
  directory > @

[] > app
  wrapper.made > answer
```

The first arm is a `Φ.directory` even at `wrapper.made`, where the read falls
through the wrapper's body and the runtime stamps `made` with the directory
it was found on, not with the wrapper. The source declares this in `holds`,
and anything else seen there is a bug in the tool.

**Rule 4. An arm that is the `^` of an anonymous formation is what its
readers put there.** A formation written as an argument belongs to nobody:

```eo
[] > foo
  x > @
    [^]
      5 > five
  [y] > x
    $.y.five > @
```

The `[^]` sits inside `foo`, but its `^` is filled by the first dotted read
of it, `$.y` inside `x`, so it is an `x` and not a `foo`. Nothing is declared
for it, and the answer comes from the readers alone.

**Rule 5. An arm that is an input is what this call passed,** and is dead
where this call passed nothing:

```eo
[^ cant-check] > is-symlink
  ^.stat.code.eq 0 > ok
  ok.if > @
    ^.stat.mode.eq 40960
    cant-check

[] > app
  f.is-symlink "hi" > first
  f.is-symlink 42 > second
  f.is-symlink.if > third
    f.unlink
    f.rmdir
```

The first arm is a `Φ.bool` at every call. The second is what each call put
into `cant-check`: a `Φ.string` at `first`, a `Φ.number` at `second`, and
nothing at `third`, where the arm is dead, since a run that takes it reads an
empty void and stops. A formation written inline in that place is what it
reduces to.

**Rule 6. The general answer of a formation is the union of its real calls.**
Asked of `is-symlink` itself rather than of one call on it, the answer is what
every call in the program made it return, joined: a `Φ.bool`, a `Φ.string` or
a `Φ.number` for the three calls above. There is no fourth caller to wait for.

**Rule 7. An arm that terminates is dead** and takes no part in the agreement
or in the choice (#8946):

```eo
[] > directory
  [^] > tmpfile
    ^.exists.if > @
      file "tmp"
      T "cannot make it"
```

`T` never hands anything back, so `tmpfile` is a `Φ.file`.

**Rule 8. A read an arm cannot answer kills that arm.** Taking a name an arm
does not have stops the program on that arm, so the other arms answer:

```eo
[] > app
  flag.if > chosen
    "text"
    42
  chosen.trimmed > answer
```

`chosen` is a `Φ.string` or a `Φ.number`. A number has no `trimmed`, so a run
that chose `42` stops at that read, and `answer` is what `"text".trimmed` is,
a `Φ.string`. An arm nobody can see into is another matter: it stays unknown,
and unknown is not dead.

Four things the rules lean on, said once.

Arms *agree* when they are the same formation after alias reduction, which is
the name `Reduced` wrote on the row. Standing on a shared ancestor is not
agreement: `Φ.string` and `Φ.number` both stand on `Φ.bytes`, and a call that
is one or the other is a choice of the two and not a `Φ.bytes`. A page may
show the ancestor; the row keeps the arms.

Rule 3 is applied before rule 5. A declared `^` sits among the voids like any
input, and the `if` that chooses on it rarely fills it, yet it is never empty,
so it is never dead.

A void is *filled* where a call put something into it, where the source
declared what it holds, as `? > size /Q.number` does and as the `^` of every
attribute does, or where an atom is declared to hand into it, as
`? > scope /{Q.chunk}` does. Only a void none of those reaches is empty.

A union is written whole, however many members it has. The page caps what it
lists; the table does not, since a reader of the table wants the fact.

Rules 1, 3, 4 and 7 are what the passes do today. Rule 2 stops instead of
answering (#8980), rule 5 keeps an empty input as a live unknown arm (#8981),
rule 6 is read for one member and refused for several (#8982), rule 8 empties
the whole read where one arm lacks the name (#8881). Each of those gets a
pack in `inference-packs` the day it lands, and this paragraph shrinks with it.

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
