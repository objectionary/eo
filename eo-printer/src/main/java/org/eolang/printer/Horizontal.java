/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.Optional;

/**
 * An application spelled on a single line.
 *
 * <p>The base, its inlined arguments and its suffix are glued together
 * into one line, which exists only when every argument has a one-line
 * spelling of its own (see {@link Arguments#joined()}).</p>
 *
 * <p>A {@code :label} suffix (#6563) binds to whatever token sits
 * immediately before it, so gluing it straight onto the end of the
 * inlined arguments would silently rebind it from this head to the last
 * argument on reparse. The base and its arguments are bracketed first in
 * that case, so the label reads back attached to the group, matching what
 * it named before printing.</p>
 *
 * @since 0.57.0
 */
final class Horizontal {

    /**
     * The rendered head of the object.
     */
    private final String base;

    /**
     * The rendered suffix, possibly empty.
     */
    private final String tail;

    /**
     * The arguments of the application.
     */
    private final Arguments args;

    /**
     * Ctor.
     *
     * @param head The rendered head
     * @param suffix The rendered suffix
     * @param arguments The arguments
     */
    Horizontal(final String head, final String suffix, final Arguments arguments) {
        this.base = head;
        this.tail = suffix;
        this.args = arguments;
    }

    /**
     * Render the line at the given indentation level.
     *
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The single line, or empty if inlining is impossible
     */
    Optional<String> print(final Style style, final int indent) {
        return this.args.joined().map(
            text -> {
                final String glued;
                if (this.tail.startsWith(":")) {
                    glued = "(".concat(this.base).concat(" ").concat(text)
                        .concat(")").concat(this.tail);
                } else {
                    glued = this.base.concat(" ").concat(text).concat(this.tail);
                }
                return style.indent(indent).concat(glued);
            }
        );
    }
}
