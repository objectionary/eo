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
 * <p>The blanks trailing the block are handed back, though. A blank
 * standing between the last skipped line and the line the walk resumes
 * at separates two objects instead of sitting inside either, and §6.5
 * reads its rules off exactly such separators — R-6.5.4 forbids one in
 * front of a plain object. Swallowing it would drop that diagnostic
 * from whatever follows the failed block, since the line never reaches
 * the blank-line bookkeeping. Blanks with skipped lines still under
 * them are nested and stay skipped, and a block running to the end of
 * the file keeps its trailing blanks, no object being left for them to
 * separate.</p>
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
     *
     * @param lines All lines of the program, in source order
     */
    Recovery(final List<Span> lines) {
        this.spans = lines;
    }

    /**
     * The index the walk resumes at, having failed on the line at
     * {@code failed}.
     *
     * @param failed Index of the line that failed to parse
     * @return Index of the resumption point
     */
    int after(final int failed) {
        return this.skip(failed + 1, this.spans.get(failed).indent());
    }

    /**
     * The index the walk resumes at, skipping forward from {@code from}
     * while a line still belongs to the block of a line at {@code indent}.
     * Unlike {@link #after(int)}, the scan start and the reference indent
     * are given separately — needed when the failed line's own index does
     * not carry its indent (e.g. a merged multi-line literal, where the
     * block belongs to the head line but scanning must start past every
     * already-merged continuation line).
     *
     * @param from Index to start scanning from
     * @param indent Indent of the line whose block is being skipped
     * @return Index of the resumption point
     */
    int skip(final int from, final int indent) {
        int idx = from;
        while (idx < this.spans.size() && this.skipped(idx, indent)) {
            idx = idx + 1;
        }
        if (idx < this.spans.size()) {
            idx = this.rewound(idx, from - 1);
        }
        return idx;
    }

    private int rewound(final int stop, final int floor) {
        int idx = stop;
        while (idx - 1 > floor && this.spans.get(idx - 1).blank()) {
            idx = idx - 1;
        }
        return idx;
    }

    private boolean skipped(final int idx, final int indent) {
        final Span span = this.spans.get(idx);
        return span.blank() || span.indent() > indent;
    }
}
