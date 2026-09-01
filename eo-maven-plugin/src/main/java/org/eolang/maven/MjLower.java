/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.eolang.lowering.Phino;

/**
 * Compute the constant fragments of a program at build time.
 *
 * <p>An expression decided by data alone, such as {@code 1.plus 1}, pays
 * the full object-graph cost at runtime for a value the compiler could
 * know. This goal computes such fragments through the external
 * {@code phino} binary and splices the values back as literals, so the
 * graphs are never built. It runs after {@link MjMerge} and before
 * {@link MjTranspile}, reading the XMIR of each object and repointing it
 * at the rewritten copy in {@link Lowering#DIR} — only when something in
 * it was actually folded.</p>
 *
 * <p>The goal is part of the normal chain but soft by default: without a
 * {@code phino} of the pinned version on the PATH it warns once and does
 * nothing, so a machine without it builds fine, only without the
 * folding. Setting {@code eo.loweringRequired} turns that skip into a
 * build failure, which is what our own CI does, so that a release is
 * never silently unlowered. Setting {@code eo.lowering} to false turns
 * the goal off entirely.</p>
 *
 * <p>Whether lowering ran changes the Java that {@link MjTranspile}
 * eventually generates from the same sources, so the goal leaves
 * {@link Lowering#MARKER} behind saying what ran, and the transpile
 * cache key folds that file in — two machines with and without phino
 * then never share a slot. When the goal skips or is disabled, the
 * marker is removed.</p>
 *
 * @since 0.76.0
 * @todo #8137:45min Install phino in <code>.rultor.yml</code> and pass
 *  <code>-Deo.loweringRequired=true</code> in its merge and release
 *  scripts, so that a release is never silently unlowered. The image
 *  rultor builds in has no Haskell toolchain and no build cache, so this
 *  needs either a prebuilt phino binary or an image that carries one.
 */
@Mojo(
    name = "lower",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjLower extends MjSafe {

    /**
     * The most rewriting steps one dataization may take, enough for any
     * fragment a human writes and little enough that a diverging one is
     * refused in milliseconds.
     */
    private static final int STEPS = 10_000;

    /**
     * Whether constant fragments are folded at all.
     */
    @Parameter(property = "eo.lowering", defaultValue = "true")
    private boolean lowering;

    /**
     * Whether a missing or mismatched phino binary fails the build
     * instead of skipping the goal.
     */
    @Parameter(
        alias = "loweringRequired",
        property = "eo.loweringRequired",
        defaultValue = "false"
    )
    private boolean demanded;

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
     * Ctor.
     */
    public MjLower() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final Path home = this.targetDir.toPath().resolve(Lowering.DIR);
        final Path marker = home.resolve(Lowering.MARKER);
        if (this.lowering) {
            final Phino phino = new Phino(this.binary, MjLower.STEPS);
            if (phino.suitable()) {
                try (TjsForeign tojos = this.tojos()) {
                    new Timed(
                        new Lowering(tojos.standalone(), home, phino)
                    ).exec();
                }
                new Saved(
                    String.format(
                        "lower-%s-%s",
                        phino.pin(),
                        new Fingerprint("/org/eolang/lowering/universe.phi").get()
                    ),
                    marker
                ).value();
            } else {
                this.skipped(marker, phino);
            }
        } else {
            Files.deleteIfExists(marker);
            Logger.info(this, "Lowering is disabled by eo.lowering");
        }
    }

    private void skipped(final Path marker, final Phino phino) throws IOException {
        if (this.demanded) {
            throw new IllegalStateException(
                String.format(
                    "The phino binary '%s' is absent or not of version %s, while eo.loweringRequired is set",
                    this.binary,
                    phino.pin()
                )
            );
        }
        Files.deleteIfExists(marker);
        Logger.warn(
            this,
            "The phino binary '%s' is absent or not of version %s, so no constant fragment is folded",
            this.binary,
            phino.pin()
        );
    }
}
