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
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.text.TextOf;
import org.eolang.printer.PenaltyKey;
import org.eolang.printer.Xmir;

/**
 * Print XMIR to EO.
 *
 * <p>This goal goes through all XMIR sources found in the specified directory,
 * converts them back to EO format, and saves the resulting EO files
 * in the specified output directory, preserving the original directory structure.
 * Input XMIR files are found in {@link #sources},
 * output EO files are saved in {@link #output}.</p>
 *
 * @since 0.33.0
 */
@Mojo(
    name = "print",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class MjPrint extends MjSafe {

    /**
     * Pattern to catch the trailing .xmir extension.
     */
    private static final Pattern XMIR = Pattern.compile("\\.xmir$");

    /**
     * Directory with XMIR sources to print.
     */
    @Parameter(
        alias = "printSourcesDir",
        property = "eo.printSourcesDir",
        required = true,
        defaultValue = "${project.basedir}/src/main/xmir"
    )
    private File sources;

    /**
     * Directory where printed EO files are placed.
     */
    @Parameter(
        alias = "printOutputDir",
        property = "eo.printOutputDir",
        required = true,
        defaultValue = "${project.build.directory}/generated-sources/eo"
    )
    private File output;

    /**
     * Points charged for each level of indentation on a line.
     */
    @Parameter(alias = "penaltyIndent", property = "eo.penaltyIndent")
    private Integer indent;

    /**
     * Points charged for each opening parenthesis.
     */
    @Parameter(alias = "penaltyBracket", property = "eo.penaltyBracket")
    private Integer bracket;

    /**
     * Points charged for each character past the allowed width.
     */
    @Parameter(alias = "penaltyExcess", property = "eo.penaltyExcess")
    private Integer excess;

    /**
     * The column after which characters start being charged.
     */
    @Parameter(property = "eo.width")
    private Integer width;

    /**
     * Ctor.
     */
    public MjPrint() {
        // nothing
    }

    @Override
    void exec() throws IOException {
        final int total = new Threaded<>(
            new Walk(this.sources.toPath()).includes(Set.of("**.xmir")),
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
        final Path home = this.output.toPath();
        final Path relative = Paths.get(
            MjPrint.XMIR.matcher(
                this.sources.toPath().relativize(source).toString()
            ).replaceFirst(".eo")
        );
        new Saved(
            new Xmir(
                new XMLDocument(new TextOf(source).asString()), this.weights()
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

    /**
     * Assemble the overridden penalty weights from the Maven properties.
     *
     * <p>Only the properties that the user actually set are put into the
     * map; every absent key falls back to its {@link PenaltyKey#fallback()}
     * default inside the printer.</p>
     *
     * @return The weights, keyed by {@link PenaltyKey}
     */
    private Map<PenaltyKey, Integer> weights() {
        final Map<PenaltyKey, Integer> map = new EnumMap<>(PenaltyKey.class);
        if (this.indent != null) {
            map.put(PenaltyKey.INDENT, this.indent);
        }
        if (this.bracket != null) {
            map.put(PenaltyKey.BRACKET, this.bracket);
        }
        if (this.excess != null) {
            map.put(PenaltyKey.EXCESS, this.excess);
        }
        if (this.width != null) {
            map.put(PenaltyKey.WIDTH, this.width);
        }
        return map;
    }
}
