/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * The place where the walk carries on after a line failed to parse — §7.
 *
 * <p>Every line indented deeper than a failed one belongs to it. Parsing
 * such a line on its own only piles up errors the source never made,
 * since an orphaned child collides with the indent stack instead of
 * standing under its parent. This object holds the lines of the program
 * and answers where the walk picks up again: the first line standing
 * back at or above the indent of the one that failed. A blank line
 * carries no indent of its own and never ends the skipped block.</p>
 *
 * @since 0.1
 */
final class Recovery {

    /**
     * All lines of the program, in source order.
     */
    private final List<Span> spans;

    /**
     * Ctor.
     * @param lines All lines of the program, in source order
     */
    Recovery(final List<Span> lines) {
        this.spans = lines;
    }

    /**
     * The index the walk resumes at, having failed on the line at
     * {@code failed}.
     * @param failed Index of the line that failed to parse
     * @return Index of the resumption point
     */
    int after(final int failed) {
        final int indent = this.spans.get(failed).indent();
        int idx = failed + 1;
        while (idx < this.spans.size() && this.skipped(idx, indent)) {
            idx = idx + 1;
        }
        return idx;
    }

    /**
     * Whether the line at {@code idx} still belongs to the block of a
     * line that failed at {@code indent}.
     * @param idx Index of the line
     * @param indent Indent of the failed line
     * @return True when the line must be skipped
     */
    private boolean skipped(final int idx, final int indent) {
        final Span span = this.spans.get(idx);
        return span.blank() || span.indent() > indent;
    }
}
