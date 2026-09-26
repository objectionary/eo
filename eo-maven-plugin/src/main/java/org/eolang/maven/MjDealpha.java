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
 * Mojo that gives the positional arguments of a program the names of the
 * voids they land in, right after the {@code inference} goal.
 *
 * <p>The parser has no way to know which void an argument fills, so it names
 * them by their places: in {@code foo k}, the {@code k} is {@code α0}. The
 * tables of {@link MjInference} know better, since every application there
 * says which void of what it copies each argument goes to. This goal reads
 * them and writes {@code foo (bar: k)} instead, wherever the answer is
 * certain, as described in #8301.</p>
 *
 * <p>An argument keeps its place where no single void is known to take it,
 * and the number of those is written to the log. With {@link #rigid} set,
 * the build fails when there is one.</p>
 *
 * <p>The XMIR goes to {@link Dealphaing#DIR}, and {@link MjTranspile} reads it
 * from there.</p>
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
     * The directory with the tables that {@link MjInference} writes.
     */
    @Parameter(
        alias = "inferenceDir",
        property = "eo.inferenceDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/6-inference"
    )
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
        try (TjsForeign tojos = this.tojos()) {
            new Timed(
                new Dealphaing(
                    tojos.standalone(),
                    new Landings(this.tables.toPath().resolve("links.xml")),
                    this.target.toPath().resolve(Dealphaing.DIR),
                    this.rigid
                )
            ).exec();
        }
    }
}
