/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import java.io.FileNotFoundException;
import java.nio.file.Path;

/**
 * The tree the {@code parse} goal left behind, as the {@code format} goal
 * needs to read it.
 *
 * <p>Two things stand between the XMIR in {@link Parsing#DIR} and the tree
 * {@link MjFormat} prints from. The parse goal homes a bare reference into
 * the program's own package, which the printer would spell back out into a
 * source that never carried it, so
 * {@code /org/eolang/maven/format/unhome-package.xsl} takes that prefix off
 * again. And the XMIR waits on disk laid out for a human to read, so the
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
     * Ctor.
     * @param path The XMIR the parse goal wrote
     */
    Unhomed(final Path path) {
        this.file = path;
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
            new StClasspath("/org/eolang/maven/format/unhome-package.xsl")
        ).pass(parsed);
    }
}
