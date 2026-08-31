/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

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
 * <p>The XMIR prepared for the rules is saved in {@link #prepared} and the
 * tables in {@link #tables}, a document each. Not one of them fails the
 * build. The pages a reader opens are drawn by {@link MjInferenceReport},
 * a goal of its own, from those same two directories.</p>
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
     * The directory where the XMIR prepared for the rules is saved.
     */
    @Parameter(
        alias = "preInferenceDir",
        property = "eo.preInferenceDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/6-pre-inference"
    )
    private File prepared;

    /**
     * The directory where the tables are saved.
     */
    @Parameter(
        alias = "inferenceDir",
        property = "eo.inferenceDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/6-inference"
    )
    private File tables;

    /**
     * Ctor.
     */
    public MjInference() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        new Timed(
            new Inferring(
                this.targetDir.toPath().resolve(Parsing.DIR),
                this.prepared.toPath(),
                this.tables.toPath()
            )
        ).exec();
    }
}
