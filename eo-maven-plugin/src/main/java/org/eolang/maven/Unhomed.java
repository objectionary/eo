/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.FileNotFoundException;
import java.nio.file.Path;

/**
 * The tree the {@code parse} goal left in {@link Parsing#DIR}, as the
 * {@code format} goal needs to read it.
 *
 * <p>Two things stand between the two. The parse goal homes a bare
 * reference to a local package object into the program's own package, and
 * the printer would spell that prefix out into a source that never carried
 * it, so {@code unhome-package.xsl} takes it back off, told the same names
 * the parse goal was told so that it undoes exactly that and nothing else.
 * And the XMIR waits on disk laid out for a human to read, so the
 * indentation between its elements arrives as text of its own, which the
 * printer would carry into the EO it prints; the blanks go before the sheet
 * runs.</p>
 *
 * @since 0.62.0
 */
final class Unhomed {

    /**
     * The XMIR the parse goal wrote.
     */
    private final Path file;

    /**
     * Space separated qualified names of the local package objects.
     */
    private final String objects;

    /**
     * Ctor.
     * @param path The XMIR the parse goal wrote
     * @param names Space separated qualified names of the local package objects
     */
    Unhomed(final Path path, final String names) {
        this.file = path;
        this.objects = names;
    }

    /**
     * The tree of it.
     * @return The tree, as the format goal needs to read it
     * @throws FileNotFoundException If the XMIR is not there
     */
    XML tree() throws FileNotFoundException {
        final XML parsed = new XMLDocument(this.file);
        for (final XML blank : parsed.nodes("//text()[not(normalize-space())][../*]")) {
            blank.inner().getParentNode().removeChild(blank.inner());
        }
        return new Xsline(
            new TrDefault<Shift>(
                new StClasspath(
                    "/org/eolang/maven/format/unhome-package.xsl",
                    String.format("objects %s", this.objects)
                )
            )
        ).pass(parsed);
    }
}
