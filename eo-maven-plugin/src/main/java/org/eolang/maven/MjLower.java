/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.eolang.lowering.Lowering;

/**
 * Fold the formations of a program into Java atoms.
 *
 * <p>A formation whose behaviour is decided at compile time pays the full
 * cost of the object graph at runtime, for an answer the compiler could
 * have known. Lowering computes such a formation through the external
 * {@code phino} binary, which is the only thing here that knows the
 * calculus, and keeps the answer as the body of a Java atom, so that the
 * graph is never built while the program runs.</p>
 *
 * <p>This goal is the entry point of that pipeline. It runs after
 * {@code inference-report}, by which time a program has said everything
 * the lowering needs to hear about it, and before {@code transpile}, the
 * last moment at which the Java of a program can still be changed.</p>
 *
 * <p>Nothing is folded yet. The goal is off unless {@code eo.lowering}
 * turns it on, and when it is on it only makes sure that the binary on
 * this machine is the one the pin names, since an answer of another
 * version cannot be trusted.</p>
 *
 * @since 0.74.0
 */
@Mojo(
    name = "lower",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjLower extends MjSafe {

    /**
     * Whether formations are lowered at all.
     */
    @Parameter(property = "eo.lowering", defaultValue = "false")
    private boolean lowering;

    /**
     * The name or path of the phino executable.
     */
    @Parameter(
        alias = "phinoBinary",
        property = "eo.phinoBinary",
        defaultValue = "phino"
    )
    private String binary;

    /**
     * The directory with the tables of {@code eo:inference}.
     */
    @Parameter(
        alias = "inferenceDir",
        property = "eo.inferenceDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/6-inference"
    )
    private File tables;

    /**
     * The directory where the lowering keeps what it makes.
     */
    @Parameter(
        alias = "loweringDir",
        property = "eo.loweringDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/7-lower"
    )
    private File home;

    /**
     * Ctor.
     */
    public MjLower() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        if (this.lowering) {
            try (TjsForeign tojos = this.tojos()) {
                new Lowering(
                    new ListOf<>(new Mapped<>(TjForeign::xmir, tojos.standalone())),
                    this.tables.toPath(),
                    this.home.toPath(),
                    this.binary
                ).exec();
            }
        } else {
            Logger.info(
                this,
                "Lowering is disabled, turn it on with -Deo.lowering=true"
            );
        }
    }
}
