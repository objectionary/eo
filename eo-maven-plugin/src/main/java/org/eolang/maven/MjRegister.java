/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;

/**
 * Find and register all {@code .eo} sources in the "foreign" catalog.
 *
 * <p>
 *     This goal scans the {@code <sourcesDir>} directory for all {@code .eo} files
 *     matching the inclusion and exclusion GLOB filters specified in the
 *     {@code <includeSources>} and {@code <excludeSources>} parameters respectively.
 *     By default, it includes all {@code .eo} files found recursively.
 *     Each found EO source is then registered in the "foreign" catalog that later processed by
 *     other goals like {@link MjParse}, {@link MjAssemble} or {@link MjCompile}.
 *     This goal only changes the "foreign" catalog and does not save any generated files.
 * </p>
 *
 * @since 0.12
 */
@Mojo(
    name = "register",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true
)
@SuppressWarnings("PMD.ImmutableField")
public final class MjRegister extends MjSafe {
    /**
     * Pattern for .eo files.
     */
    private static final Pattern PATTERN = Pattern.compile("^[a-zA-Z0-9\\-]+\\.eo$");

    /**
     * List of inclusion GLOB filters for finding EO files
     * in the {@code <includeSources>} directory, which can be
     * pretty global (or even a root one).
     *
     * @implNote {@code property} attribute is omitted for collection
     * properties since there is no way of passing it via command line.
     * @checkstyle MemberNameCheck (15 lines)
     */
    @Parameter
    private Set<String> includeSources = new SetOf<>("**.eo");

    /**
     * List of exclusion GLOB filters for finding EO files
     * in the {@code &lt;includeSources&gt;} directory, which can be
     * pretty global (or even a root one).
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter
    private Set<String> excludeSources = new SetOf<>();

    /**
     * Whether it should fail on file names not matching required pattern.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.strictFileNames",
        required = true,
        defaultValue = "true"
    )
    private boolean strictFileNames = true;

    @Override
    public void exec() {
        if (this.sourcesDir == null) {
            throw new IllegalArgumentException(
                String.format("sourcesDir is null. Please specify a valid sourcesDir for %s", this)
            );
        }
        this.removeOldFiles();
        final int before = this.scopedTojos().size();
        if (before > 0) {
            Logger.info(this, "There are %d EO sources registered already", before);
        }
        final Unplace unplace = new Unplace(this.sourcesDir);
        final int registered = new Threaded<>(
            new Walk(this.sourcesDir.toPath())
                .includes(this.includeSources)
                .excludes(this.excludeSources),
            file -> {
                if (this.strictFileNames
                    && !MjRegister.PATTERN.matcher(file.getFileName().toString()).matches()) {
                    throw new IllegalArgumentException(
                        String.format(
                            "Incorrect name found: '%s'. EO name must match '%s'",
                            file.getFileName().toString(),
                            MjRegister.PATTERN
                        )
                    );
                }
                final String name = unplace.make(file);
                if (this.scopedTojos().contains(name)) {
                    Logger.debug(this, "EO source %s already registered", name);
                } else {
                    this.scopedTojos()
                        .add(name)
                        .withSource(file.toAbsolutePath())
                        .withHash(new ChSource(file));
                    Logger.debug(this, "EO source %s registered", name);
                }
                return 1;
            }
        ).total();
        Logger.info(
            this,
            "Registered %d EO sources from %[file]s to %[file]s, included %s, excluded %s",
            registered, this.sourcesDir, this.foreign, this.includeSources, this.excludeSources
        );
    }

    private void removeOldFiles() {
        final File[] files = {
            this.foreign,
            this.targetDir.toPath().resolve(MjPull.DIR).toFile(),
            this.targetDir.toPath().resolve(MjResolve.DIR).toFile(),
        };
        for (final File file : files) {
            if (file.exists()) {
                new Deleted(file).get();
            }
        }
    }
}
