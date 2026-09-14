<div align="center">

# 🅴🅾 — EO

### A Pure Object-Oriented Language, Experimental

*Based on 𝜑-calculus. No types. No classes. No NULL. No mutability. No mercy.*

[![EO principles respected here](https://www.elegantobjects.org/badge.svg)](https://www.elegantobjects.org)
[![DevOps By Rultor.com](https://www.rultor.com/b/objectionary/eo)](https://www.rultor.com/p/objectionary/eo)
[![We recommend IntelliJ IDEA](https://www.elegantobjects.org/intellij-idea.svg)](https://www.jetbrains.com/idea/)

[![mvn-linux](https://github.com/objectionary/eo/actions/workflows/mvn.yml/badge.svg)](https://github.com/objectionary/eo/actions/workflows/mvn.yml)
[![PDD status](https://www.0pdd.com/svg?name=objectionary/eo)](https://www.0pdd.com/p?name=objectionary/eo)
[![Maintainability](https://qlty.sh/gh/objectionary/projects/eo/maintainability.svg)](https://qlty.sh/gh/objectionary/projects/eo)
[![Maven Central](https://img.shields.io/maven-central/v/org.eolang/eo-parser.svg)](https://mvnrepository.com/artifact/org.eolang/eo-parser)
[![codecov](https://codecov.io/gh/objectionary/eo/branch/master/graph/badge.svg)](https://codecov.io/gh/objectionary/eo)
![Lines-of-Code](https://raw.githubusercontent.com/objectionary/eo/gh-pages/loc-badge.svg)
[![Hits-of-Code](https://hitsofcode.com/github/objectionary/eo)](https://hitsofcode.com/view/github/objectionary/eo)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/objectionary/eo/blob/master/LICENSE.txt)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fcqfn%2Feo.svg?type=shield)](https://app.fossa.com/reports/0ebb3149-4934-4565-bf6f-6fa41aed3b49)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=objectionary_eo&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=objectionary_eo)

[Quick Start](#-quick-start) •
[Tutorial](#-a-taste-of-eo) •
[Architecture](#-architecture) •
[Grammar](#-grammar) •
[Contributing](#-how-to-contribute)

</div>

---

## 💭 Why EO Exists

**EO** (short for *Elegant Objects*, and also the ISO 639-1 code for [Esperanto](https://en.wikipedia.org/wiki/Esperanto)) is an object-oriented programming language grounded in [𝜑-calculus][phi-calculus].

We looked at the popular "semi-OOP" languages — [Java], [Ruby], [C++], [Python], [C#] — and even the purer ones like [Smalltalk], [Eiffel], [Self], and [Io] — and concluded: none of them go far enough. So EO draws a hard line and **removes, entirely**, the features we believe corrupt object-oriented design:

| ❌ Removed | Why it's harmful |
|---|---|
| Types | [Explanation →](https://www.yegor256.com/2020/11/10/typing-without-types.html) |
| Static/class methods & attributes | [Explanation →](http://www.yegor256.com/2014/05/05/oop-alternative-to-utility-classes.html) |
| Classes | [Explanation →](http://www.yegor256.com/2016/09/20/oop-without-classes.html) |
| Implementation inheritance | [Explanation →](http://www.yegor256.com/2016/09/13/inheritance-is-procedural.html) |
| Mutability | [For →](http://www.yegor256.com/2014/06/09/objects-should-be-immutable.html) / [Against →](https://www.yegor256.com/2016/09/07/gradients-of-immutability.html) |
| `NULL` | [Explanation →](http://www.yegor256.com/2014/05/13/why-null-is-bad.html) |
| Global scope | [Explanation →](https://www.yegor256.com/2018/07/03/global-variables.html) |
| Type casting | [Explanation →](http://www.yegor256.com/2015/04/02/class-casting-is-anti-pattern.html) |
| Reflection | [Explanation →](https://www.yegor256.com/2022/06/05/reflection-means-hidden-coupling.html) |
| Scalar types & data primitives | — |
| Annotations | [Explanation →](http://www.yegor256.com/2016/04/12/java-annotations-are-evil.html) |
| Operators | — |
| Traits & mixins | [Explanation →](https://www.yegor256.com/2017/03/07/traits-and-mixins.html) |
| Flow control (`for`, `while`, `if`, …) | — |

In EO, **everything is an object**, and objects only ever interact by composing and decorating one another.

---

## 🚀 Quick Start

**1.** Install [Java SE] and [npm].

**2.** Install [eoc], the EO compiler:

```bash
npm install -g eolang@0.37.1
```

**3.** Write your first program, `app.eo`:

```eo
# Just prints hello.

[args] > app
  stdout > @
    "Hello, world!\n"
```

**4.** Compile it (first run may take a minute):

```bash
eoc --easy link
```

**5.** Run it:

```bash
eoc --easy --alone dataize app
```

You should see:

```text
Hello, world!
```

---

## 📖 A Taste of EO

### Abstract objects & decoration

The program above defines a new [**abstract object**][abstract objects] named `app`, with a single attribute `@`. That attribute holds a copy of `stdout`, created with one argument, `"Hello, world!"`.

`stdout` is itself abstract — it can't be used on its own until it's *copied* with the required arguments:

```text
stdout
  "Hello, world!\n"
```

> 💡 **Indentation matters in EO**, exactly like Python: two spaces per nesting level.

The same code can be written **horizontally**:

```text
stdout "Hello, world!"
```

### Grouping arguments

Brackets group arguments and remove ambiguity. Here, `stdout` receives the result of copying `printf` with two arguments:

```eo
# Says hello to Jeff.

stdout > [] > app
  "Hello, %s!".printf
    * "Jeffrey"
```

...or, horizontally:

```eo
[] > app
  stdout ("Hello, %s!".printf (* "Jeffrey")) > @
```

### The `@` (φ) attribute

`@` always denotes the object being [**decorated**][composable decorators]. In the example above, `app` decorates a copy of `stdout` — meaning `app` transparently inherits all of `stdout`'s attributes, while still being free to define its own:

```eo
# Says hello to Jeff.

[] > app
  stdout (msg "Jeffrey") > @
  [name] > msg
    "Hello, %s!".printf (* name) > @
```

Now `app` has two attached attributes: `@` (the decorated `stdout`) and `msg` (a local abstract object taking one void attribute, `name`).

### Iteration, EO-style

With no `for`/`while` keywords, looping is done through recursive objects like `malloc` and `while`:

```eo
[args] > app
  malloc.for > @
    2
    [^ x] >>
      while > @
        ^.x.as-number.lt 6 > [^ i] >>
        seq * > [^ i] >>
          stdout
            "%d x %1$d = %d\n".printf
              *
                ^.x.as-number
                ^.x.as-number.times ^.x.as-number
          ^.x.put
            ^.x.as-number.plus 1
```

Output:

```text
2 x 2 = 4
3 x 3 = 9
4 x 4 = 16
5 x 5 = 25
```

---

## 📐 Grammar

EO's grammar lives in [`eo-parser/PARSER_SPEC.md`][parser-spec] — a spec-driven, line-by-line classification of every legal shape, with numbered rules (`R-N.M`) referenced directly by the parser implementation in `eo-parser/src/main/java/org/eolang/parser/`, which converts EO source straight to [XMIR] in a single pass, with **no intermediate AST**.

---

## 🏗 Architecture

EO's design departs from conventional compiler architecture at almost every layer:

| Layer | Traditional approach | EO's approach |
|---|---|---|
| **Formal model** | [Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) ([Haskell](https://www.haskell.org/)) or class-based models ([Java](https://en.wikipedia.org/wiki/Java_(programming_language)), [C++](https://en.wikipedia.org/wiki/C%2B%2B)) | [**𝜑-calculus**][phi-calculus] — every entity is an object with named attributes, formed by applying objects to void attributes |
| **Intermediate representation** | In-memory AST ([GCC](https://gcc.gnu.org/), [LLVM](https://llvm.org/), [javac](https://openjdk.org/projects/compiler-grammar/)) | [**XMIR**](https://news.eolang.org/2022-11-25-xmir-guide.html) — every compilation artifact is a serializable, [XSD-governed](https://www.eolang.org/XMIR.xsd) XML document, inspectable with standard XML tools |
| **Transformations** | Visitor passes in the host language ([Eclipse JDT](https://eclipse.dev/jdt/), [Roslyn](https://github.com/dotnet/roslyn)) | Pipelines of [**XSLT 2.0**](https://www.w3.org/TR/xslt20/) stylesheets — each step is a self-contained, independently testable `.xsl` file |
| **Build integration** | Custom toolchains ([Rust](https://www.rust-lang.org/)'s `cargo`, [Go](https://go.dev/)'s `go build`) | **Apache Maven** — the `eo-maven-plugin` exposes mojos (`parse`, `assemble`, `transpile`, …) running during `generate-sources` / `process-sources` |
| **Dependency resolution** | Binary artifact repositories ([Maven Central](https://search.maven.org/), [npm](https://www.npmjs.com/)) | [**Objectionary**](https://github.com/objectionary/home) — a Git-hosted registry of canonical objects; the `MjPull` mojo fetches and caches `.eo` *sources*, never opaque binaries |
| **Standard library** | Written in the host/implementation language | **Written in EO itself** — `bytes`, `number`, `string`, `tuple`, `seq`, and friends live as plain `.eo` files in `eo-runtime/src/main/eo/`, compiled by the same pipeline that compiles user code |

This self-hosting constraint is deliberate: it forces the compiler and runtime to stay correct for the very subset of EO the standard library depends on, and it means any contributor can read and modify primitive behavior without ever touching Java.

Because everything a programmer expresses is an object — reused only through **decoration**, never inheritance — EO has no types, no classes, no static methods, no implementation inheritance, no `NULL`, and no operators, by construction rather than by convention.

---

## 🧭 What's Next?

- 💬 Join [our Telegram group](https://t.me/eolang_org)
- 🎥 Watch the [intro video](https://www.youtube.com/watch?v=QaKIw1Bh3Oc)
- 📰 Read [our blog], especially the [recently published papers][papers]
- 📄 Learn [XMIR], the XML dialect we use to represent EO programs — see the [XSD] and the [spec][XMIR HTML]
- 📦 Browse the full collection of canonical objects at [objectionary][home]
- 🔧 Read more about [integration with Maven][eo-maven-plugin]

---

## 🤝 How to Contribute

1. **Fork** the repository and make your changes.
2. Before opening a pull request, run the full [Maven] build:

   ```bash
   mvn clean install -Pqulice
   ```

   You'll need **[Maven] 3.3+** and **[Java] 21+** for the `qulice` profile (17+ without it), plus **[git]** on your `PATH` — some quality checks shell out to it. If you use [xcop], make sure it's `0.8.0`+.

3. Follow these conventions:

   - 🌿 Name your branch after the issue, e.g. `42`
   - 📝 Prefix commits with `fix(#42):` followed by a short description
   - ✂️ Keep pull requests between **40–100 hits of code** (lines added + deleted) — split larger changes into puzzles via [PDD]
   - 🔗 Reference the resolved issue, e.g. `Closes #42`
   - 📣 Ping **@yegor256** in the PR description

We'll review and merge into `master` as soon as it clears our quality bar.

---

## 👥 Contributors

[![contributors](https://contributors-img.web.app/image?repo=objectionary/eo)](https://github.com/objectionary/eo/graphs/contributors)

## 🙏 Special Thanks

We use the **[YourKit Java Profiler]** to keep EO's components fast:

[![YourKit](https://www.yourkit.com/images/yklogo.png)](https://www.yourkit.com)

---

<div align="center">
<sub>Licensed under MIT · Built with 𝜑-calculus, XSLT, and stubbornness</sub>
</div>

[abstract objects]: https://www.yegor256.com/2020/12/01/abstract-objects.html
[book]: http://www.yegor256.com/elegant-objects.html
[C#]: https://en.wikipedia.org/wiki/C_Sharp_%28programming_language%29
[C++]: https://en.wikipedia.org/wiki/C%2B%2B
[composable decorators]: https://www.yegor256.com/2015/02/26/composable-decorators.html
[Eiffel]: https://en.wikipedia.org/wiki/Eiffel_(programming_language)
[eo-maven-plugin]: https://github.com/objectionary/eo/tree/master/eo-maven-plugin
[eoc]: https://github.com/objectionary/eoc
[Esperanto]: https://en.wikipedia.org/wiki/Esperanto
[git]: https://git-scm.com
[guidelines]: https://www.yegor256.com/2014/04/15/github-guidelines.html
[home]: https://github.com/objectionary/home
[Io]: https://en.wikipedia.org/wiki/Io_(programming_language)
[Java SE]: https://www.oracle.com/java/technologies/downloads/
[Java]: https://en.wikipedia.org/wiki/Java_%28programming_language%29
[Maven]: https://maven.apache.org
[npm]: https://docs.npmjs.com/downloading-and-installing-node-js-and-npm
[our blog]: https://news.eolang.org
[papers]: https://news.eolang.org/papers.html
[parser-spec]: https://github.com/objectionary/eo/blob/master/eo-parser/PARSER_SPEC.md
[PDD]: https://github.com/yegor256/0pdd
[phi-calculus]: https://arxiv.org/abs/2111.13384
[Python]: https://en.wikipedia.org/wiki/Python_%28programming_language%29
[Ruby]: https://en.wikipedia.org/wiki/Ruby_%28programming_language%29
[Self]: https://en.wikipedia.org/wiki/Self_(programming_language)
[Smalltalk]: https://en.wikipedia.org/wiki/Smalltalk
[xcop]: https://github.com/yegor256/xcop
[XMIR HTML]: https://www.eolang.org/XMIR.html
[XMIR]: https://news.eolang.org/2022-11-25-xmir-guide.html
[XSD]: https://www.eolang.org/XMIR.xsd
[YourKit Java Profiler]: https://www.yourkit.com/java/profiler
