<!-- markdownlint-disable MD013 MD033 MD041 MD043 -->

<img alt="logo" src="https://www.objectionary.com/cactus.svg" height="100px" />

# eo-lowering

Folds an EO formation whose answer a compiler can already work out into a
Java atom, so that the object graph behind it is never built while the
program runs.

The whole world of a build is merged into one document for the external
`phino` binary, and every entry of it, one per formation to fold, is morphed
by a run of its own, side by side with the others. What comes back from each
run is a protocol of every operation that fired, one XML file per object in
`target/eo/7-lowering-protocols`, which becomes the body of one generated
Java class per folded formation.

Nothing is folded yet. The pipeline is six stages: the tests are cut out of
the sources, since each of them is a program of its own and not part of the
world, the entries are planted, the world is merged, the entries are morphed,
and the puzzle in each of the last two says how the sources will be patched
and the Java rendered.
