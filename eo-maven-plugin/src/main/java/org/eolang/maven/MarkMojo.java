/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Extend the current list of foreign objects with those
 * visible in resolved artifacts.
 *
 * @since 0.11
 */
@Mojo(
    name = "extend",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MarkMojo extends SafeMojo {
    @Override
    public void exec() throws IOException {
        final Path home = this.targetDir.toPath().resolve(ResolveMojo.DIR);
        if (Files.exists(home)) {
            final Collection<String> deps = new DepDirs(home);
            long found = 0;
            for (final String dep : deps) {
                final Path sub = home.resolve(dep).resolve(CopyMojo.DIR);
                if (Files.exists(sub)) {
                    found += this.scan(sub, dep.split(Pattern.quote(File.separator))[3]);
                }
            }
            Logger.info(
                this, "New %d EO objects found in %d unpacked dependencies",
                found, deps.size()
            );
        }
    }

    /**
     * Take sources from EO-SOURCES dir and register them in the CSV.
     *
     * @param dir Where they are
     * @param version The version of the JAR
     * @return How many registered
     */
    private long scan(final Path dir, final String version) {
        final Unplace unplace = new Unplace(dir);
        final Collection<Path> sources = new Walk(dir);
        final TjsForeign tojos = this.scopedTojos();
        final long done = sources.stream()
            .filter(src -> src.toString().endsWith(".eo"))
            .map(unplace::make)
            .map(tojos::add)
            .peek(tojo -> tojo.withVersion(version))
            .count();
        Logger.info(
            this,
            "Found %d .eo source file(s) in %[file]s, %d program(s) registered with version %s",
            sources.size(), dir, done, version
        );
        return done;
    }
}
