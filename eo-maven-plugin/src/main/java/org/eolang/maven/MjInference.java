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
 * <p>
 *     The checker looks for one kind of mistake: an attribute is taken from an
 *     object that certainly doesn't have it. In this program, for example,
 *     {@code t} does have {@code next}, but the object attached to {@code next}
 *     has no {@code foo}:
 * </p>
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 * <p>
 *     One pass through the XMIR of every file parsed so far fills the tables
 *     with one rule per kind of object. A formation says what its object
 *     provides, and that the list is complete. A reference ({@code ξ.t}) says
 *     that one type is a copy of another. A dispatch ({@code .next}) says that
 *     the object it is taken from must have that attribute. An application
 *     files a pending check: this argument must fit into that void.
 * </p>
 * <p>
 *     Then the checks are drained one by one, each of them either deciding,
 *     splitting into smaller checks, or waiting for facts that may never come.
 *     A mistake is reported only when the object that misses an attribute is
 *     complete, so that parts of the program this goal cannot see — atoms,
 *     delegation through {@code φ} — make it silent rather than wrong.
 * </p>
 * <p>
 *     Only the first of those rules is implemented so far, together with the
 *     preparation the rest of them will need. The XMIR prepared for the rules
 *     is saved in {@link #prepared} and the tables in
 *     {@link #tables}, a document each.
 * </p>
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
        property = "eo.preInferenceDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/6-pre-inference"
    )
    private File prepared;

    /**
     * The directory where the tables are saved.
     */
    @Parameter(
        property = "eo.inferenceDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/6-inference"
    )
    private File tables;

    @Override
    void exec() throws IOException {
        new Inferring(
            this.targetDir.toPath().resolve(Parsing.DIR),
            this.prepared.toPath(),
            this.tables.toPath()
        ).exec();
    }
}
