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
 * Draw a page per source file from what {@link MjInference} wrote.
 *
 * <p>A page shows the author's own source with a mark on every object,
 * coloured by one of ten bands of what the engine answers there in
 * {@code answers.xml}: green where it answers one name and nothing else,
 * other colours where it answers several names, names with gaps, only gaps,
 * or a read that fails. An index lists the pages with a bar per file, and
 * the goal logs how many objects fall in each band and writes the same
 * numbers down in {@code ladder.txt}, beside the directory of the tables.</p>
 *
 * <p>A goal of its own rather than a flag on {@link MjInference}: a build
 * that wants the pages asks for this goal, one that does not never runs
 * it.</p>
 *
 * @since 0.71.0
 */
@Mojo(
    name = "inference-report",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjInferenceReport extends MjSafe {

    /**
     * The directory where the pages are written.
     */
    @Parameter(
        alias = "inferenceReportDir",
        property = "eo.inferenceReportDir",
        required = true,
        defaultValue = "${project.build.directory}/site/inference"
    )
    private File pages;

    /**
     * The directory where the XMIR prepared for the engine was saved, falling
     * back to the "pre-inference" directory {@code Seqdir} already found
     * when unset.
     */
    @Parameter(alias = "preInferenceDir", property = "eo.preInferenceDir")
    private File prepared;

    /**
     * The directory where {@code answers.xml} was saved, falling back to the
     * "inference" directory {@code Seqdir} already found when unset.
     */
    @Parameter(alias = "inferenceDir", property = "eo.inferenceDir")
    private File tables;

    /**
     * Ctor.
     */
    public MjInferenceReport() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final Path prepped;
        if (this.prepared == null) {
            prepped = new Seqdir(this.target.toPath(), 2).dirs().find("pre-inference")
                .orElse(this.target.toPath().resolve("pre-inference"));
        } else {
            prepped = this.prepared.toPath();
        }
        final Path inference;
        if (this.tables == null) {
            inference = new Seqdir(this.target.toPath(), 2).dirs().find("inference")
                .orElse(this.target.toPath().resolve("inference"));
        } else {
            inference = this.tables.toPath();
        }
        new Reporting(prepped, inference, this.pages.toPath()).exec();
    }
}
