/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import io.github.sekator778.seqdir.Seqdir;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Mapped;
import org.eolang.inference.Names;

/**
 * Mojo that reads what a program says about its own objects,
 * preferably after the {@code assemble} goal.
 *
 * <p>What a program says about its objects is written down into
 * {@code facts.xml}, which a later goal can read. In this program, for
 * example, {@code t} has {@code next}, the object attached to {@code next}
 * has nothing, and {@code inc} reads {@code foo} off whatever it is
 * given:</p>
 *
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 *
 * <p>One pass through the XMIR of every file parsed so far writes every
 * formation with its attributes, every name read and what it is read off,
 * the head of every chain, and every call with its arguments.</p>
 *
 * <p>Nothing here says whether the program is wrong. Judging it was written
 * and taken out again in #6661, because a verdict needs the object that
 * misses an attribute to have been seen whole, and almost none of them have
 * been; the program has to be described before anything can read it for
 * mistakes.</p>
 *
 * <p>The XMIR of every registered source is read, and not every file the
 * parse directory holds: a source that was deleted leaves its XMIR behind,
 * and the facts would go on describing an object nobody wrote any more
 * (#9271). Where each one lies is worked out from the identifier of the
 * tojo, and not read off its {@code xmir} attribute, because {@code eo:merge}
 * moves that attribute of a package object into the merge directory, while
 * the facts are made of what the parser wrote.</p>
 *
 * <p>The engine then reads {@code facts.xml} and writes {@code answers.xml}
 * beside it: for every place, which objects it can be when Java runs the
 * program. It has to know three lists of names of the program, which are
 * parameters here, with the names of eo-runtime as defaults:
 * {@link #canon}, {@link #roots} and {@link #scalars}.</p>
 *
 * <p>The prepared XMIR is saved in {@link #prepared}, and {@code facts.xml}
 * and {@code answers.xml} in {@link #tables}. Nothing of it fails the
 * build.</p>
 *
 * @since 0.67.0
 */
@Mojo(
    name = "inference",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjInference extends MjSafe {

    /**
     * The directory where the prepared XMIR is saved, falling back to a
     * directory {@code Seqdir} numbers "pre-inference" when unset.
     */
    @Parameter(alias = "preInferenceDir", property = "eo.preInferenceDir")
    private File prepared;

    /**
     * The directory where {@code facts.xml} and {@code answers.xml} are
     * saved, falling back to a directory {@code Seqdir} numbers "inference"
     * when unset.
     */
    @Parameter(alias = "inferenceDir", property = "eo.inferenceDir")
    private File tables;

    /**
     * The kinds that are one shared copy each, born {@code canon}: the first
     * holds the data, and every other one holds the first. Each must have
     * one void, {@code @}.
     */
    @Parameter(
        alias = "inferenceCanon",
        property = "eo.inferenceCanon",
        defaultValue = "Φ.bytes,Φ.number,Φ.string"
    )
    private List<String> canon;

    /**
     * The packages of list code: a copy that other code makes of a
     * formation under one of them starts a tag, which keeps apart what list
     * code does for different places of user code.
     */
    @Parameter(
        alias = "inferenceRoots",
        property = "eo.inferenceRoots",
        defaultValue = "Φ.tuple,Φ.map,Φ.set"
    )
    private List<String> roots;

    /**
     * The packages whose formations give back new data, so that a copy of
     * one of them takes no tag.
     */
    @Parameter(
        alias = "inferenceScalars",
        property = "eo.inferenceScalars",
        defaultValue =
            "Φ.number,Φ.bytes,Φ.string,Φ.bool,Φ.true,Φ.false,Φ.i8,Φ.i16,Φ.i32,Φ.i64,Φ.u8,Φ.u16,Φ.u32,Φ.u64,Φ.dataized"
    )
    private List<String> scalars;

    /**
     * Ctor.
     */
    public MjInference() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final Path parsed = new Seqdir(this.target.toPath(), 2).dirs().once("parse");
        final Path prepped;
        if (this.prepared == null) {
            prepped = new Seqdir(this.target.toPath(), 2).dirs().once("pre-inference");
        } else {
            prepped = this.prepared.toPath();
        }
        final Path inference;
        if (this.tables == null) {
            inference = new Seqdir(this.target.toPath(), 2).dirs().once("inference");
        } else {
            inference = this.tables.toPath();
        }
        try (TjsForeign tojos = this.tojos()) {
            new Inferring(
                new Mapped<>(
                    tojo -> new Place(tojo.identifier()).make(parsed, MjAssemble.XMIR),
                    tojos.withXmir()
                ),
                parsed,
                prepped,
                inference,
                new Names(this.canon, this.roots, this.scalars)
            ).exec();
        }
    }
}
