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
 * Draw a page per source file from the tables {@link MjInference} wrote.
 *
 * <p>The three numbers the tables come with say how much of a program we
 * understand without saying which part. A page says both, with the author's
 * own source on it and a mark on every object: green where the formation can
 * be named, amber where the answer is somebody else's void, red where there
 * is nothing.</p>
 *
 * <p>A goal of its own rather than a flag on {@link MjInference}: a build
 * that wants the pages asks for this goal, one that does not never runs it.</p>
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
     * The directory where the XMIR prepared for the rules was saved.
     */
    @Parameter(
        alias = "preInferenceDir",
        property = "eo.preInferenceDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/6-pre-inference"
    )
    private File prepared;

    /**
     * The directory where the tables were saved.
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
    public MjInferenceReport() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        new Timed(
            new Reporting(this.prepared.toPath(), this.tables.toPath(), this.pages.toPath())
        ).exec();
    }
}
