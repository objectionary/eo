/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Generate the atom return types table from XMIR sources.
 * <p>
 * Walks the XMIR sources in {@link #atomsInputDir}, extracts the
 * {@code forma} of every lambda atom together with its declared return
 * type, and writes the result as a CSV file at {@link #atomsOutput}.
 * Each output line has the form {@code <forma>,<return-type>}; entries
 * are sorted by {@code forma} to make the file stable across builds.
 * </p>
 * @since 0.57
 */
@Mojo(
    name = "atoms-table",
    defaultPhase = LifecyclePhase.PROCESS_CLASSES,
    threadSafe = true
)
public final class MjAtomsTable extends MjSafe {

    /**
     * XSL that extracts atom entries from a single XMIR file.
     */
    private static final String XSL = "/org/eolang/maven/atoms-table.xsl";

    /**
     * Directory with XMIR sources to scan for atoms.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.atomsInputDir",
        required = true,
        defaultValue = "${project.build.directory}/eo/1-parse"
    )
    private File atomsInputDir;

    /**
     * Output CSV file with the atoms table.
     * @checkstyle MemberNameCheck (10 lines)
     */
    @Parameter(
        property = "eo.atomsOutput",
        required = true,
        defaultValue = "${project.build.outputDirectory}/org/eolang/atoms.csv"
    )
    private File atomsOutput;

    @Override
    void exec() throws IOException {
        final Path home = this.atomsInputDir.toPath();
        if (!Files.isDirectory(home)) {
            Logger.info(
                this,
                "No XMIR sources at %[file]s, atoms table will be empty",
                home
            );
            this.write(new TreeMap<>());
            return;
        }
        final Map<String, String> table = new TreeMap<>();
        final Xsline xsline = new Xsline(new StClasspath(MjAtomsTable.XSL));
        for (final Path source : new Walk(home)) {
            final XML before = new XMLDocument(
                new UncheckedText(new TextOf(source)).asString()
            );
            final Xnav after = new Xnav(xsline.pass(before).inner());
            after.path("/atoms/atom").forEach(
                atom -> table.put(
                    atom.attribute("forma").text().orElseThrow(
                        () -> new IllegalStateException(
                            "atom element must carry 'forma' attribute"
                        )
                    ),
                    atom.attribute("returns").text().orElseThrow(
                        () -> new IllegalStateException(
                            "atom element must carry 'returns' attribute"
                        )
                    )
                )
            );
        }
        this.write(table);
        Logger.info(
            this,
            "Wrote %d atom return type(s) to %[file]s",
            table.size(),
            this.atomsOutput.toPath()
        );
    }

    /**
     * Write the atoms table to the output CSV file.
     * @param table Forma to return-type mappings, already sorted
     * @throws IOException If write fails
     */
    private void write(final Map<String, String> table) throws IOException {
        final Path target = this.atomsOutput.toPath();
        Files.createDirectories(target.getParent());
        final StringBuilder out = new StringBuilder();
        for (final Map.Entry<String, String> entry : table.entrySet()) {
            out.append(entry.getKey())
                .append(',')
                .append(entry.getValue())
                .append('\n');
        }
        Files.writeString(target, out.toString(), StandardCharsets.UTF_8);
    }
}
