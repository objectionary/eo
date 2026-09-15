/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

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
 * @todo #8548:60min Move the phino version check and its pin into a new
 *  eo-lowering Maven module, and make this goal call one Lowering step of
 *  that module, which composes the stages of the pipeline: boxes, entries,
 *  world, run, patch, and render. Until then this goal only verifies phino
 *  and creates its folder.
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
            this.verify(new Phino(this.binary));
        } else {
            Logger.info(
                this,
                "Lowering is disabled, turn it on with -Deo.lowering=true"
            );
        }
    }

    private void verify(final Phino phino) throws IOException {
        final String pinned = phino.pin();
        final String found;
        try {
            found = phino.version();
        } catch (final IOException ex) {
            throw new IllegalStateException(
                String.format(
                    "The binary '%s' cannot run, while lowering needs phino %s",
                    this.binary,
                    pinned
                ),
                ex
            );
        }
        if (!found.equals(pinned)) {
            throw new IllegalStateException(
                String.format(
                    "The binary '%s' is of version %s, while lowering needs phino %s",
                    this.binary,
                    found,
                    pinned
                )
            );
        }
        Files.createDirectories(this.home.toPath());
        Logger.info(
            this,
            "Phino %s is found at '%s', though nothing is lowered yet",
            pinned,
            this.binary
        );
    }
}
