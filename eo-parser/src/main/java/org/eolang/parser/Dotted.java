/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A dotted path written in a meta directive.
 *
 * <p>Every dot introduces a segment, so a leading dot, a trailing dot and a
 * doubled dot each name a segment that is not there. Such a path is copied
 * verbatim into a base by `expand-aliases.xsl`, where it names nothing at
 * all, which is why it is caught while the line is still being read.</p>
 *
 * @since 0.74.0
 */
final class Dotted {

    /**
     * The path.
     */
    private final String path;

    /**
     * Ctor.
     * @param text The path, as the source wrote it
     */
    Dotted(final String text) {
        this.path = text;
    }

    /**
     * Whether the path names a segment that is not there.
     * @return True if a dot of it introduces nothing
     */
    boolean broken() {
        return this.path.startsWith(".")
            || this.path.endsWith(".")
            || this.path.contains("..");
    }
}
