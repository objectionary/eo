/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import io.github.sekator778.seqdir.Seqdir;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Mapped;

/**
 * Mojo that works out the types of the objects of a program,
 * preferably after the {@code assemble} goal.
 *
 * <p>What a program says about its own types is written down, file by file,
 * into tables a later goal can read. In this program, for example, {@code t}
 * has {@code next}, the object attached to {@code next} has nothing, and
 * {@code inc} takes {@code foo} from whatever it is given:</p>
 *
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 *
 * <p>One pass through the XMIR of every file parsed so far fills the tables
 * with one rule per kind of object. A formation says what its object
 * provides, and that the list is complete. A reference ({@code ξ.t}) says
 * that one type is a copy of another. A dispatch ({@code .next}) says that
 * the object it is taken from must have that attribute.</p>
 *
 * <p>Nothing here says whether the program is wrong. Judging it was written
 * and taken out again in #6661, because a verdict needs the object that
 * misses an attribute to have been seen whole, and almost none of them have
 * been; the tables have to describe the program before anything can read
 * them for mistakes.</p>
 *
 * <p>The XMIR of every registered source is read, and not every file the
 * parse directory holds: a source that was deleted leaves its XMIR behind,
 * and the tables would go on describing an object nobody wrote any more
 * (#9271). Where each one lies is worked out from the identifier of the
 * tojo, and not read off its {@code xmir} attribute, because {@code eo:merge}
 * moves that attribute of a package object into the merge directory, while
 * the tables are made of what the parser wrote.</p>
 *
 * <p>The XMIR prepared for the rules is saved in {@link #prepared} and the
 * tables in {@link #tables}, a document each, with {@code ladder.txt} beside
 * them saying how much of the program they turned out to describe. Not one of
 * them fails the build. The pages a reader opens are drawn by
 * {@link MjInferenceReport}, a goal of its own, from those same two
 * directories.</p>
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
     * The directory where the XMIR prepared for the rules is saved, falling
     * back to a directory {@code Seqdir} numbers "pre-inference" when unset.
     */
    @Parameter(alias = "preInferenceDir", property = "eo.preInferenceDir")
    private File prepared;

    /**
     * The directory where the tables are saved, falling back to a directory
     * {@code Seqdir} numbers "inference" when unset.
     */
    @Parameter(alias = "inferenceDir", property = "eo.inferenceDir")
    private File tables;

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
                inference
            ).exec();
        }
    }
}
