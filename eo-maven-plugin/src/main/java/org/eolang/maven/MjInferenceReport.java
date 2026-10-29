/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Draw a page per source file from what {@link MjInference} wrote.
 *
 * <p>A page shows the author's own source with a mark on every object,
 * coloured by how well the inference names it. A goal of its own rather
 * than a flag on {@link MjInference}: a build that wants the pages asks for
 * this goal, one that does not never runs it.</p>
 *
 * <p>The pages were drawn from the tables of the old engine, which are gone
 * with it, so for now the goal draws nothing.</p>
 *
 * @since 0.71.0
 * @todo #9650:90min Draw the pages of the inference report again. Port the
 *  report the Python engine draws to Java, so that this goal reads
 *  facts.xml, answers.xml and the prepared XMIR of the "pre-inference"
 *  directory and writes one coloured page per source file, with an index,
 *  into target/site/inference. Then turn the "inference" workflow back on,
 *  since it publishes those pages.
 */
@Mojo(
    name = "inference-report",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjInferenceReport extends MjSafe {

    /**
     * Ctor.
     */
    public MjInferenceReport() {
        // nothing
    }

    @Override
    void exec() {
        Logger.info(this, "The inference report is not drawn until it is ported to the new engine");
    }
}
