/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import javax.xml.transform.stream.StreamSource;
import org.cactoos.iterable.Sorted;

/**
 * The boxing of every formation of the build.
 *
 * <p>Every XMIR file of the build is copied into the {@code boxed/}
 * directory with {@code boxing.xsl}, which plants a box on each formation that
 * has a body: a lambda next to that body, so that entering it is a fire the
 * atom engine is asked to serve, and the name of the box is how every stage
 * after this one knows which formation it is looking at. Nothing else of a
 * file changes, which is what lets the patch of a later stage take the boxes
 * out again and arrive back at the source the author wrote.</p>
 *
 * <p>A box is numbered across the whole world and not within a file, since
 * the files are one document by the time the calculus sees them. So the
 * files are boxed in a fixed order, each of them counting from what the ones
 * before it planted, and it is the stylesheet alone that decides which
 * formation is worth a box: this class only counts the boxes it finds in
 * what came back.</p>
 *
 * @since 0.74.0
 */
final class Boxing implements Stage {

    /**
     * The XMIR files of the build.
     */
    private final Collection<Path> sources;

    /**
     * The directory with the tables of {@code eo:inference}.
     */
    private final Path tables;

    /**
     * The directory where the lowering keeps what it makes.
     */
    private final Path home;

    /**
     * Ctor.
     *
     * @param srcs The XMIR files of the build
     * @param tbls The directory with the tables of {@code eo:inference}
     * @param dir The directory where the lowering keeps what it makes
     */
    Boxing(final Collection<Path> srcs, final Path tbls, final Path dir) {
        this.sources = srcs;
        this.tables = tbls;
        this.home = dir;
    }

    @Override
    public void exec() throws IOException {
        if (!Files.exists(this.tables.resolve("provides.xml"))) {
            throw new IllegalStateException(
                String.format(
                    "There is no 'provides.xml' in '%s', while boxing needs the tables of eo:inference to say what a formation comes back with",
                    this.tables
                )
            );
        }
        final Path boxed = this.home.resolve("boxed");
        Files.createDirectories(boxed);
        final XSL sheet = new XSLDocument(
            Boxing.class.getResource("/org/eolang/lowering/boxing.xsl"),
            "/org/eolang/lowering/boxing.xsl"
        ).with((href, base) -> new StreamSource(href))
            .with("inference", this.tables.toUri().toString());
        final Collection<String> names = new HashSet<>(0);
        int boxes = 0;
        for (final Path source : new Sorted<>(this.sources)) {
            final XML after = sheet.with("start", boxes).transform(new XMLDocument(source));
            final String name = String.format(
                "%s.xmir", after.xpath("/object/o/@loc").get(0).replaceAll("^Φ\\.", "")
            );
            if (!names.add(name)) {
                throw new IllegalStateException(
                    String.format(
                        "Two sources of the build are both boxed into '%s', while '%s' holds one file per object",
                        name,
                        boxed
                    )
                );
            }
            Files.write(boxed.resolve(name), after.toString().getBytes(StandardCharsets.UTF_8));
            boxes += after.xpath("//o[@name='λ'][starts-with(text(), 'L_box_')]/text()").size();
        }
        Logger.info(
            this,
            "Boxed %d formations of %d files into %[file]s",
            boxes,
            this.sources.size(),
            boxed
        );
    }
}
