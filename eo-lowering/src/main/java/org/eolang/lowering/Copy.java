/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * The place of a copy of one XMIR source, inside a directory of copies.
 *
 * <p>Two objects of one name may live in two packages, and both are legal
 * EO, so the file name alone does not tell their copies apart. This is why
 * a copy goes under the directories of its package, the way the parser
 * puts the XMIR itself: the source of {@code +package foo} named
 * {@code app.xmir} has its copy at {@code foo/app.xmir}.</p>
 *
 * @since 0.64.0
 */
final class Copy {

    /**
     * The path of the source.
     */
    private final Path source;

    /**
     * The XMIR of the source.
     */
    private final XML xmir;

    /**
     * Ctor.
     *
     * @param src The path of the source
     * @param xml The XMIR of the source
     */
    Copy(final Path src, final XML xml) {
        this.source = src;
        this.xmir = xml;
    }

    /**
     * The path of the copy, relative to the directory of copies.
     *
     * @return The directories of the package, then the file name of the source
     */
    Path relative() {
        Path path = Paths.get("");
        for (final String part : String.join(
            "", this.xmir.xpath("/object/metas/meta[head='package']/tail/text()")
        ).split("\\.")) {
            if (!part.isEmpty()) {
                path = path.resolve(part);
            }
        }
        return path.resolve(this.source.getFileName().toString());
    }
}
