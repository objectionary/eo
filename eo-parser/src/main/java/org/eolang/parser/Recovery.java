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
 * carries no indent of its own, so it is resolved by looking past it to
 * the next real line: genuinely nested inside the block if that line is
 * indented deeper, but a file-level separator in front of an unrelated
 * sibling otherwise — and a separator does not extend the skip.</p>
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
            idx = this.rewound(idx, failed);
        }
        return idx;
    }

    /**
     * Step back over the run of blank lines trailing the skipped block,
     * so the walk meets them again as the separators they are.
     * @param stop Index the skip stopped at
     * @param failed Index of the line that failed to parse
     * @return Index of the first blank of the trailing run
     */
    private int rewound(final int stop, final int failed) {
        int idx = stop;
        while (idx - 1 > failed && this.spans.get(idx - 1).blank()) {
            idx = idx - 1;
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
        final boolean result;
        if (span.blank()) {
            result = this.blankBelongsToBlock(idx, indent);
        } else {
            result = span.indent() > indent;
        }
        return result;
    }

    /**
     * Whether a blank line at {@code idx} still belongs to the block of a
     * line at {@code indent} — true only when the next non-blank line is
     * itself indented deeper than {@code indent}. A blank line whose next
     * real line stands back at or above {@code indent} is a file-level
     * separator between the block and an unrelated sibling, not part of
     * the block, and must not be skipped.
     * @param idx Index of the blank line
     * @param indent Indent of the failed line
     * @return True when the blank line must be skipped
     */
    private boolean blankBelongsToBlock(final int idx, final int indent) {
        int next = idx + 1;
        while (next < this.spans.size() && this.spans.get(next).blank()) {
            next = next + 1;
        }
        return next < this.spans.size() && this.spans.get(next).indent() > indent;
    }
}
