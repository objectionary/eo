/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.FileNotFoundException;
import java.nio.file.Path;

/**
 * The tree the {@code parse} goal walked out of a source and left in
 * {@link Parsing#DIR}.
 *
 * <p>It waits on disk laid out for a human to read, so the indentation
 * between its elements arrives as text of its own. {@link MjFormat} prints
 * the tree back into EO, and the printer would carry those blanks into
 * what it prints, so they go here before anyone reads the tree.</p>
 *
 * @since 0.62.0
 */
final class Walked {

    /**
     * The XMIR the parse goal wrote.
     */
    private final Path file;

    /**
     * Ctor.
     * @param path The XMIR the parse goal wrote
     */
    Walked(final Path path) {
        this.file = path;
    }

    /**
     * The tree of it.
     * @return The tree, with no blanks left between its elements
     * @throws FileNotFoundException If the XMIR is not there
     */
    XML tree() throws FileNotFoundException {
        final XML parsed = new XMLDocument(this.file);
        for (final XML blank : parsed.nodes("//text()[not(normalize-space())][../*]")) {
            blank.inner().getParentNode().removeChild(blank.inner());
        }
        return parsed;
    }
}
