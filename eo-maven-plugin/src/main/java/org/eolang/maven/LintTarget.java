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
     * Ctor.
     *
     * @param doc The XMIR document being linted
     * @param src The document's own source path
     */
    LintTarget(final XML doc, final Path src) {
        this.xmir = doc;
        this.source = src;
    }

    /**
     * The lint result's path, under the given base directory.
     *
     * @param base Base directory
     * @return Full path
     */
    Path under(final Path base) {
        return new Place(
            new OnDetailed(new OnDefault(new Xnav(this.xmir.inner())), this.source).get()
        ).make(base, MjAssemble.XMIR);
    }
}
