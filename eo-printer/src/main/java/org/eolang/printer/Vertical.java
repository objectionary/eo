/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import java.util.List;

/**
 * A head on one line with its children laid out beneath it.
 *
 * <p>This is the rendering every object always has: the head — the base
 * with whatever suffix it carries — stays on the current line and each
 * child goes on a line of its own, indented one level deeper, spelling
 * itself through {@link Node#indented(Style, int)}. A child that carries
 * the bare {@code !} of an anonymous inline const (#5821) takes an
 * auto-name there, since the marker has no spelling on a line of its own
 * and this is the one place that puts a child there. A child a method
 * continuation ({@code .y}) hangs on is laid out vertically whatever the
 * penalties say, since the continuation has nothing to attach to under a
 * one-line application (#8058).</p>
 *
 * @since 0.57.0
 */
final class Vertical {

    /**
     * The rendered head, base and suffix together.
     */
    private final String head;

    /**
     * The children (arguments or bindings), in order.
     */
    private final List<Node> kids;

    /**
     * Ctor.
     * @param line The rendered head
     * @param children The children, in order
     */
    Vertical(final String line, final List<Node> children) {
        this.head = line;
        this.kids = children;
    }

    /**
     * Render the block at the given indentation level.
     * @param style The style to lay out in
     * @param indent The indentation level
     * @return The rendered block
     */
    String print(final Style style, final int indent) {
        final StringBuilder block = new StringBuilder(style.indent(indent))
            .append(this.head);
        for (int idx = 0; idx < this.kids.size(); ++idx) {
            final Node kid = this.kids.get(idx);
            if (idx + 1 < this.kids.size() && this.kids.get(idx + 1).continuation()) {
                block.append(kid.stacked(style, indent + 1));
            } else {
                block.append(kid.indented(style, indent + 1));
            }
        }
        return block.toString();
    }
}
