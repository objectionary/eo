/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.text.TextOf;
import org.eolang.parser.Xmir;

/**
 * Print XMIR to EO.
 * @since 0.33.0
 */
@Mojo(
    name = "print",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjPrint extends MjSafe {
    /**
     * Directory with XMIR sources to print.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.printSourcesDir",
        required = true,
        defaultValue = "${project.basedir}/src/main/xmir"
    )
    private File printSourcesDir;

    /**
     * Directory where printed EO files are placed.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.printOutputDir",
        required = true,
        defaultValue = "${project.build.directory}/generated-sources/eo"
    )
    private File printOutputDir;

    @Override
    void exec() throws IOException {
        final Path home = this.printOutputDir.toPath();
        final int total = new Threaded<>(
            new Walk(this.printSourcesDir.toPath()),
            source -> {
                final Path relative = Paths.get(
                    this.printSourcesDir.toPath().relativize(source).toString()
                        .replace(".xmir", ".eo")
                );
                final String program = new Xmir(
                    new XMLDocument(new TextOf(source).asString())
                ).toEO();
                new Saved(program, home.resolve(relative)).value();
                Logger.info(
                    this,
                    "Printed: %[file]s (%[size]s) => %[file]s (%[size]s)",
                    source,
                    source.toFile().length(),
                    this.printOutputDir.toPath().resolve(relative),
                    this.printOutputDir.toPath().resolve(relative).toFile().length()
                );
                return 1;
            }
        ).total();
        if (total == 0) {
            Logger.info(this, "No XMIR sources found");
        } else {
            Logger.info(this, "Printed %d XMIR sources into EO", total);
        }
    }
}
