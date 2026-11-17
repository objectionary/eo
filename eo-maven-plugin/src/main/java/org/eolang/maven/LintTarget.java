/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.nio.file.Path;
import org.eolang.parser.OnDefault;
import org.eolang.parser.OnDetailed;

/**
 * The path a per-file lint result belongs at, derived from the XMIR
 * document's own object name.
 *
 * <p>A source that declares no object gives its XMIR no name at all, and
 * that very absence is the defect the linting is about to report, so the
 * name the catalog gives the file stands in for it instead of aborting the
 * goal (#9325).</p>
 *
 * @since 0.64
 */
final class LintTarget {

    /**
     * The XMIR document being linted.
     */
    private final XML xmir;

    /**
     * The document's own source path (for a detailed error location if the
     * object name can't be read).
     */
    private final Path source;

    /**
     * The name the catalog gives the file, used when the document names
     * no object of its own.
     */
    private final String catalogued;

    /**
     * Ctor.
     *
     * @param doc The XMIR document being linted
     * @param src The document's own source path
     * @param name The name the catalog gives the file
     */
    LintTarget(final XML doc, final Path src, final String name) {
        this.xmir = doc;
        this.source = src;
        this.catalogued = name;
    }

    /**
     * The lint result's path, under the given base directory.
     *
     * @param base Base directory
     * @return Full path
     */
    Path under(final Path base) {
        return new Place(this.name()).make(base, MjAssemble.XMIR);
    }

    private String name() {
        final Xnav nav = new Xnav(this.xmir.inner());
        final String name;
        if (nav.path("/object/o[1]/@name").findAny().isPresent()
            || nav.path("/object/class/@name").findAny().isPresent()) {
            name = new OnDetailed(
                new OnDefault(new Xnav(this.xmir.inner())), this.source
            ).get();
        } else {
            name = this.catalogued;
        }
        return name;
    }
}
