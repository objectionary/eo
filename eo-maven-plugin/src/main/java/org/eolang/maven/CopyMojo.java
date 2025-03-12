/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Copy all {@code .eo} files from the {@code src/main/eo} directory
 * to the {@code target/classes/EO-SOURCES} directory
 * and replace {@code 0.0.0} versions in them to the right version numbers.
 *
 * @since 0.11
 */
@Mojo(
    name = "copy",
    defaultPhase = LifecyclePhase.PREPARE_PACKAGE,
    threadSafe = true
)
@SuppressWarnings("PMD.ImmutableField")
public final class CopyMojo extends SafeMojo {

    /**
     * Dir with sources.
     */
    static final String DIR = "EO-SOURCES";

    /**
     * Replacer or version.
     */
    private static final Pattern REPLACE = Pattern.compile(
        "^(\\+rt .+):0\\.0\\.0(.*)$",
        Pattern.MULTILINE
    );

    /**
     * Target directory with resources to be packaged in JAR.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.outputDir",
        required = true,
        defaultValue = "${project.build.outputDirectory}"
    )
    private File outputDir;

    /**
     * The version to use for 0.0.0 replacements.
     *
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        property = "eo.version",
        required = true,
        defaultValue = "${project.version}"
    )
    private String version;

    @Override
    public void exec() throws IOException {
        final Path target = this.outputDir.toPath().resolve(CopyMojo.DIR);
        final Collection<Path> sources = new Walk(this.sourcesDir.toPath());
        for (final Path src : sources) {
            new Home(target).save(
                CopyMojo.REPLACE
                    .matcher(new UncheckedText(new TextOf(new InputOf(src))).asString())
                    .replaceAll(String.format("$1:%s$2", this.version)),
                Paths.get(
                    src.toAbsolutePath().toString().substring(
                        this.sourcesDir.toPath().toAbsolutePath().toString().length() + 1
                    )
                )
            );
        }
        if (sources.isEmpty()) {
            Logger.warn(
                this, "No sources copied from %[file]s to %[file]s",
                this.sourcesDir, target
            );
        } else {
            Logger.info(
                this, "%d source(s) copied from %[file]s to %[file]s",
                sources.size(), this.sourcesDir, target
            );
        }
    }

}
