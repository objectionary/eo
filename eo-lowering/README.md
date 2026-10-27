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
`2-protocols`, inside the home of the lowering at `target/eo/NN-lowering`,
which becomes the body of one generated Java class per folded formation.

The pipeline is six stages. The tests are cut out of the sources, since each
of them is a program of its own and not part of the world, and every `T`
there becomes a copy of `throw`, so that its message reaches the protocol;
the entries are planted into `1-planting`; the world is merged; the entries
are morphed into `2-protocols`; a Java atom is rendered into `3-atoms` for
every entry whose protocol says enough; and the sources are patched with
those atoms into `4-patched`, which the transpiler reads. An entry the
rendering cannot turn into Java is a taint: it keeps no atom and stays in EO
exactly as it was written.
