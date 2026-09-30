/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.time.Duration;
import java.util.Collection;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.Text;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
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
 * <p>The goal folds by default; {@code -Deo.lowering=false} turns it off.
 * When phino cannot be started on this computer, the goal prints a warning
 * and does nothing, so the build goes on without atoms. With
 * {@code -Deo.skipWithoutPhino=false} the goal fails the build instead.
 * When it runs it makes sure that the binary on this machine is the one
 * the pin names, since an answer of another version cannot be trusted,
 * and then plants the entries of the build. It hands the lowering the
 * XMIR of every standalone object, the directory where {@code eo:inference}
 * left its tables, because an entry is a formation applied to what the
 * tables say its voids hold, and the directory of the build, where the
 * lowering keeps the sources with their tests cut out in
 * {@code NN-lowering-planting}, the world in {@code NN-lowering} and the
 * protocol of every object morphed in {@code NN-lowering-protocols}.
 * The atoms the lowering renders land in {@code NN-lowering-atoms}, which
 * this goal hands to javac as a source root, and every XMIR file with an
 * atom in the place of a body lands in {@code NN-lowering-patched}, where
 * this goal points the tojo of that object, so the transpiler reads the
 * patched copy and every other object stays where it was. The copies stay
 * from build to build, so only a copy listed in {@code patched.tsv}, one
 * patched in this very build, is handed to the transpiler.</p>
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
    @Parameter(property = "eo.lowering", defaultValue = "true")
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
     * Whether the goal skips, instead of failing the build, when phino
     * cannot be started on this computer.
     */
    @Parameter(
        alias = "skipWithoutPhino",
        property = "eo.skipWithoutPhino",
        defaultValue = "true"
    )
    private boolean optional;

    /**
     * The seconds one run of phino may take on one entry before it is killed.
     */
    @Parameter(
        alias = "loweringBudget",
        property = "eo.loweringBudget",
        defaultValue = "10"
    )
    private int budget;

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
     * Ctor.
     */
    public MjLower() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        if (this.lowering) {
            final Path atoms = this.target.toPath().resolve("7-lowering-atoms")
                .toAbsolutePath();
            final Path patched = this.target.toPath().resolve("7-lowering-patched")
                .toAbsolutePath();
            try (TjsForeign tojos = this.tojos()) {
                final Lowering pipeline = new Lowering(
                    new ListOf<>(new Mapped<>(TjForeign::xmir, tojos.standalone())),
                    this.tables.toPath(),
                    this.target.toPath(),
                    this.binary,
                    this.caching("lowered"),
                    atoms,
                    patched,
                    Duration.ofSeconds(this.budget)
                );
                if (this.optional && !pipeline.available()) {
                    Logger.warn(
                        this,
                        "Lowering is skipped, since phino '%s' cannot be started, set -Deo.skipWithoutPhino=false to fail instead",
                        this.binary
                    );
                } else {
                    pipeline.exec();
                    this.repoint(tojos, patched);
                    this.project.addCompileSourceRoot(atoms.toString());
                    Logger.info(
                        this, "The directory added to Maven 'compile-source-root': %[file]s", atoms
                    );
                }
            }
        } else {
            Logger.info(
                this,
                "Lowering is disabled with -Deo.lowering=false"
            );
        }
    }

    private void repoint(final TjsForeign tojos, final Path patched) {
        final Collection<String> fresh = new ListOf<>(
            new Mapped<>(
                Text::asString,
                new Split(
                    new TextOf(this.target.toPath().resolve("7-lowering/patched.tsv")),
                    "\\R"
                )
            )
        );
        for (final TjForeign tojo : tojos.standalone()) {
            final String name = tojo.xmir().getFileName().toString();
            if (fresh.contains(name)) {
                tojo.withXmir(patched.resolve(name));
            }
        }
    }
}
