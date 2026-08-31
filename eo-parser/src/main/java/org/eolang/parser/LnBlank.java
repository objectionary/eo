/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A blank line — §1.1, §6.5 of the spec.
 *
 * <p>A line whose contents are entirely whitespace (or empty). Carries no
 * indent or kind of its own (§6.5 note). The legality of a blank is
 * decided by the <em>next</em> non-blank line; this class only updates
 * the blank counters in {@link Globals} so the next line's classifier
 * can read {@code pendingBlanks()} and decide.</p>
 *
 * <p>Per R-6.5.1, blanks inside a comment block are forbidden.
 * R-6.5.3 caps the run of consecutive blanks at one; the moment a
 * second blank lands we report it here — its position is unambiguous
 * regardless of what the next non-blank line turns out to be.</p>
 *
 * <p>The run of blanks that closes the file is exempt: it separates no
 * two non-blank lines and precedes no object, so R-6.5.3 has nothing to
 * say about it. That run is the business of R-6.5.6 / R-8.4, reported
 * once at end of stream, and a blank of it that reported here too would
 * say the same thing twice (#7978).</p>
 *
 * @since 0.1
 */
final class LnBlank implements Line {

    /**
     * Source span of the blank line.
     */
    private final Span span;

    /**
     * Whether this blank belongs to the run that closes the file.
     */
    private final boolean tail;

    /**
     * Ctor.
     * @param source Source span
     * @param closing Whether this blank belongs to the run that closes the file
     */
    LnBlank(final Span source, final boolean closing) {
        this.span = source;
        this.tail = closing;
    }

    @Override
    public void into(final Stack stack, final Globals globals, final Emit emit) {
        if (!this.tail && globals.pendingBlanks() >= 1) {
            emit.error(
                this.span.line(), 0,
                "consecutive blank lines forbidden — at most one blank may separate two non-blank lines (R-6.5.3)"
            );
        }
        globals.blank();
    }
}
