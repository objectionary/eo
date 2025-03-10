/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.set.SetOf;

/**
 * Find and register all {@code .eo} sources in the "foreign" catalog.
 *
 * @since 0.12
 */
@Mojo(
    name = "register",
    defaultPhase = LifecyclePhase.GENERATE_SOURCES,
    threadSafe = true
)
@SuppressWarnings("PMD.ImmutableField")
public final class RegisterMojo extends SafeMojo {
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
        final Pattern pattern = Pattern.compile("^[a-zA-Z0-9\\-]+\\.eo$");
        final int before = this.scopedTojos().size();
        if (before > 0) {
            Logger.info(this, "There are %d EO sources registered already", before);
        }
        final Collection<Path> sources = new Walk(this.sourcesDir.toPath())
            .includes(this.includeSources)
            .excludes(this.excludeSources);
        final Unplace unplace = new Unplace(this.sourcesDir);
        for (final Path file : sources) {
            if (this.strictFileNames && !pattern.matcher(file.getFileName().toString()).matches()) {
                throw new IllegalArgumentException(
                    String.format(
                        "Incorrect name found: '%s'. EO name must match '%s'",
                        file.getFileName().toString(),
                        pattern
                    )
                );
            }
            final String name = unplace.make(file);
            if (this.scopedTojos().contains(name)) {
                Logger.debug(this, "EO source %s already registered", name);
                continue;
            }
            this.scopedTojos().add(name).withSource(file.toAbsolutePath());
            Logger.debug(this, "EO source %s registered", name);
        }
        Logger.info(
            this,
            "Registered %d EO sources from %[file]s to %[file]s, included %s, excluded %s",
            sources.size(), this.sourcesDir, this.foreign, this.includeSources, this.excludeSources
        );
    }

    private void removeOldFiles() {
        final File[] files = {
            this.foreign,
            this.targetDir.toPath().resolve(PullMojo.DIR).toFile(),
            this.targetDir.toPath().resolve(ResolveMojo.DIR).toFile(),
        };
        for (final File file : files) {
            if (file.exists()) {
                new Deleted(file).get();
            }
        }
    }
}
