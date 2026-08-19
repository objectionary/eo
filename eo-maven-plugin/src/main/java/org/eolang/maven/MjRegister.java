/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;

/**
 * Find and register all {@code .eo} sources in the "foreign" catalog.
 *
 * <p>This goal scans the {@code <sourcesDir>} directory for all {@code .eo} files
 * matching the inclusion and exclusion GLOB filters specified in the
 * {@code <includeSources>} and {@code <excludeSources>} parameters respectively.
 * By default, it includes all {@code .eo} files found recursively.
 * Each found EO source is then registered in the "foreign" catalog that later processed by
 * other goals like {@link MjParse}, {@link MjAssemble} or {@link MjCompile}.
 * This goal only changes the "foreign" catalog and does not save any generated files.</p>
 *
 * @since 0.12
 */
@Mojo(
    name = "register",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true
)
public final class MjRegister extends MjSafe {

    /**
     * Pattern for .eo files.
     */
    private static final Pattern PATTERN = Pattern.compile("^[a-zA-Z0-9\\-]+\\.eo$");

    /**
     * Ctor.
     */
    public MjRegister() {
        // nothing
    }

    /**
     * List of inclusion GLOB filters for finding EO files
     * in the {@code <includeSources>} directory, which can be
     * pretty global (or even a root one).
     * @implNote {@code property} attribute is omitted for collection
     *  properties since there is no way of passing it via command line.
     * @checkstyle MemberNameCheck (15 lines)
     */
    @Parameter(defaultValue = "**.eo")
    private Set<String> includeSources;

    /**
     * List of exclusion GLOB filters for finding EO files
     * in the {@code <includeSources>} directory, which can be
     * pretty global (or even a root one).
     * @implNote {@code defaultValue} attribute is omitted, because an empty
     *  one is not rendered into the descriptor of the plugin by
     *  the {@code maven-plugin-plugin}, thus this may stay {@code NULL}.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter
    private Set<String> excludeSources;

    /**
     * Whether it should fail on file names not matching required pattern.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.strictFileNames",
        required = true,
        defaultValue = "true"
    )
    private boolean strictFileNames;

    @Override
    public void exec() throws IOException {
        if (this.sourcesDir == null) {
            throw new IllegalArgumentException(
                String.format("sourcesDir is null. Please specify a valid sourcesDir for %s", this)
            );
        }
        try (TjsForeign tojos = this.tojos()) {
            this.removeOldFiles();
            final int before = tojos.size();
            if (before > 0) {
                Logger.info(this, "There are %d EO sources registered already", before);
            }
            final Unplace unplace = new Unplace(this.sourcesDir);
            Logger.info(
                this,
                "Registered %d EO sources from %[file]s to %[file]s, included %s, excluded %s",
                new Threaded<>(
                    new WkDefault(this.sourcesDir.toPath())
                        .includes(this.includeSources)
                        .excludes(this.excludes()),
                    file -> this.register(file, unplace, tojos)
                ).total(),
                this.sourcesDir,
                this.foreign,
                this.includeSources,
                this.excludes()
            );
        }
    }

    /**
     * Exclusion GLOB filters, as they were configured.
     * @return Set of GLOB filters, never {@code NULL}
     */
    private Set<String> excludes() {
        final Set<String> globs;
        if (this.excludeSources == null) {
            globs = new SetOf<>();
        } else {
            globs = this.excludeSources;
        }
        return globs;
    }

    /**
     * Register a single EO source file.
     * @param file Source file
     * @param unplace Unplace builder for naming
     * @param tojos The foreign catalog to register into
     * @return Always 1, to count the registered files
     */
    private int register(
        final Path file, final Unplace unplace, final TjsForeign tojos
    ) {
        if (
            this.strictFileNames
                && !MjRegister.PATTERN.matcher(file.getFileName().toString()).matches()
        ) {
            throw new IllegalArgumentException(
                String.format(
                    "Incorrect name found: '%s'. EO name must match '%s'",
                    file.getFileName().toString(),
                    MjRegister.PATTERN
                )
            );
        }
        final String name = unplace.make(file);
        if (tojos.contains(name)) {
            Logger.debug(this, "EO source %s already registered", name);
        } else {
            tojos
                .add(name)
                .withSource(file.toAbsolutePath())
                .withHash(new ChSource(file));
            Logger.debug(this, "EO source %s registered", name);
        }
        return 1;
    }

    private void removeOldFiles() {
        final File[] files = {
            this.foreign,
            this.targetDir.toPath().resolve(Pulling.DIR).toFile(),
            this.targetDir.toPath().resolve(MjResolve.DIR).toFile(),
        };
        for (final File file : files) {
            if (file.exists()) {
                new Deleted(file).get();
            }
        }
    }
}
