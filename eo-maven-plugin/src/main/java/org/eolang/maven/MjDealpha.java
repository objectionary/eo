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

/**
 * Mojo that gives the positional arguments of a program the names of the
 * voids they land in, right after the {@code inference} goal.
 *
 * <p>The parser has no way to know which void an argument fills, so it names
 * them by their places: in {@code foo k}, the {@code k} is {@code α0}. The
 * {@code answers.xml} of {@link MjInference} knows better, since the row of
 * an argument names the void it lands in whenever there is one. This goal
 * reads it and writes {@code foo (bar: k)} instead, wherever the answer is
 * certain, as described in #8301, and the {@code facts.xml} beside it says
 * that no caller can change it.</p>
 *
 * <p>An argument keeps its place where no single void is known to take it,
 * and the number of those is written to the log. With {@link #rigid} set,
 * the build fails when there is one.</p>
 *
 * <p>The XMIR goes to a directory {@code Seqdir} numbers "dealpha", and
 * {@link MjTranspile} reads it from there.</p>
 *
 * @since 0.69.0
 */
@Mojo(
    name = "dealpha",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjDealpha extends MjSafe {

    /**
     * The directory with the {@code answers.xml} and the {@code facts.xml}
     * that {@link MjInference} writes, falling back to a directory
     * {@code Seqdir} numbers "inference" when unset.
     */
    @Parameter(alias = "inferenceDir", property = "eo.inferenceDir")
    private File tables;

    /**
     * Whether to fail the build when a positional argument is left without
     * the name of its void.
     */
    @Parameter(
        alias = "failOnAlpha",
        property = "eo.failOnAlpha",
        defaultValue = "false"
    )
    private boolean rigid;

    /**
     * Ctor.
     */
    public MjDealpha() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final Path inference;
        if (this.tables == null) {
            inference = new Seqdir(this.target.toPath(), 2).dirs().once("inference");
        } else {
            inference = this.tables.toPath();
        }
        try (TjsForeign tojos = this.tojos()) {
            new Dealphaing(
                tojos.standalone(),
                new Landings(inference),
                this.target.toPath(),
                this.rigid
            ).exec();
        }
    }
}
