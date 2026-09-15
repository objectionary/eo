<!-- markdownlint-disable MD013 MD033 MD041 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

# eo-lowering

Folds an EO formation whose answer a compiler can already work out into a
Java atom, so that the object graph behind it is never built while the
program runs.

The whole world of a build goes through the external `phino` binary in one
run, and what comes back is a table of operations, which becomes the body of
one generated Java class per folded formation.

Nothing is folded yet. The pipeline is six stages, each of them empty, and
the puzzle in each one says what it will do.
