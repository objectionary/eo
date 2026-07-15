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
import org.eolang.printer.Xmir;

/**
 * Print XMIR to EO.
 * <p>
 *  This goal goes through all XMIR sources found in the specified directory,
 *  converts them back to EO format, and saves the resulting EO files
 *  in the specified output directory, preserving the original directory structure.
 *  Input XMIR files are found in {@link #printSourcesDir},
 *  output EO files are saved in {@link #printOutputDir}.
 * </p>
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
        final int total = new Threaded<>(
            new Walk(this.printSourcesDir.toPath()),
            this::print
        ).total();
        if (total == 0) {
            Logger.info(this, "No XMIR sources found");
        } else {
            Logger.info(this, "Printed %d XMIR sources into EO", total);
        }
    }

    /**
     * Print a single XMIR file as EO.
     * @param source The XMIR source path
     * @return Always 1, to count the number of printed files
     * @throws Exception If fails
     */
    private int print(final Path source) throws Exception {
        final Path home = this.printOutputDir.toPath();
        final Path relative = Paths.get(
            this.printSourcesDir.toPath().relativize(source).toString()
                .replace(".xmir", ".eo")
        );
        new Saved(
            new Xmir(
                new XMLDocument(new TextOf(source).asString())
            ).toEO(),
            home.resolve(relative)
        ).value();
        Logger.info(
            this,
            "Printed: %[file]s (%[size]s) => %[file]s (%[size]s)",
            source,
            source.toFile().length(),
            home.resolve(relative),
            home.resolve(relative).toFile().length()
        );
        return 1;
    }
}
