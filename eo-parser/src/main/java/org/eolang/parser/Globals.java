/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Global parser state — §5.1.1 of the spec.
 *
 * <p>Beyond the indent stack the parser keeps a small set of scalar flags
 * that survive across pops. They live here, not on a {@link Level},
 * because they are program-wide concerns:</p>
 *
 * <ul>
 * <li>{@link #firstObjectEmitted()} flips true once any non-meta object
 * has been parsed; consulted by R-3.2.2 (no meta after a real object).</li>
 * <li>{@link #pendingBlanks()} counts blank lines seen since the last
 * non-blank line; consumed by R-6.5 timing.</li>
 * <li>{@link #trailingBlanks()} counts blank lines since EOF; checked by
 * R-6.5.6 / R-8.4.</li>
 * <li>{@link #inTextBlock()} is true while parsing inside an open
 * triple-quoted block (§3.11).</li>
 * <li>{@link #pendingComments()} buffers comment lines awaiting
 * attachment to the next named object (§6.4).</li>
 * </ul>
 *
 * @since 0.1
 */
final class Globals {

    /**
     * The accumulated lines of the current comment block.
     */
    private final List<Span> comments;

    /**
     * True once a non-meta object has been parsed.
     */
    private boolean emitted;

    /**
     * True once the header zone is closed — the first meta directive or
     * object has landed, so no comment may appear any more (§3.3). The
     * only legal comment block sits on top of the file, before this
     * point.
     */
    private boolean closed;

    /**
     * True once at least one meta directive has been seen but no
     * non-meta non-blank line has landed yet — drives the
     * "exactly one blank between meta header and the next line"
     * rule (R-6.5.5).
     */
    private boolean meta;

    /**
     * Count of consecutive blank lines since the last non-blank line.
     */
    private int blanks;

    /**
     * Trailing-blank counter for EOF check.
     */
    private int trailing;

    /**
     * True once the top comment block has been thrown away by
     * {@link #dropComments()}. A block dropped on an error is gone for
     * good, so {@link #restore(Globals)} leaves it dropped instead of
     * bringing it back on the next line.
     */
    private boolean discarded;

    /**
     * True while inside an open triple-quoted text block.
     */
    private boolean intext;

    /**
     * Source line on which the current text block opened.
     */
    private int tline;

    /**
     * Indent (in spaces) at which the current text block opened. Used
     * to strip leading whitespace from body lines per R-3.11.2.
     */
    private int tindent;

    /**
     * Accumulator for the text block body — one entry per source line
     * between the opening and closing {@code """}.
     */
    private final List<String> tbody;

    /**
     * Ctor.
     */
    Globals() {
        this.comments = new ArrayList<>(0);
        this.tbody = new ArrayList<>(0);
        this.emitted = false;
        this.closed = false;
        this.meta = false;
        this.discarded = false;
        this.blanks = 0;
        this.trailing = 0;
        this.intext = false;
        this.tline = 0;
        this.tindent = 0;
    }

    /**
     * Whether a non-meta object has been emitted.
     * @return Flag
     */
    boolean firstObjectEmitted() {
        return this.emitted;
    }

    /**
     * Mark that a non-meta object has been emitted.
     */
    void markEmitted() {
        this.emitted = true;
    }

    /**
     * Whether the header zone is closed — no comment may appear after
     * this (§3.3).
     * @return Flag
     */
    boolean sealed() {
        return this.closed;
    }

    /**
     * Close the header zone — called once the first meta directive or
     * object lands, after any top comment block has been flushed.
     */
    void seal() {
        this.closed = true;
    }

    /**
     * Flush the pending top comment block, if any, and close the header
     * zone — idempotent once {@link #sealed()} is true. A non-empty
     * block with no blank line before the sealing line (§6.5) is
     * dropped via {@link #dropComments()} and reported as an error.
     * @param emit XMIR emitter
     * @param span Source span of the meta or object closing the header
     */
    void seal(final Emit emit, final Span span) {
        if (this.closed) {
            return;
        }
        if (!this.comments.isEmpty() && this.blanks == 0) {
            this.dropComments();
            throw new ParseError(
                span.line(), span.indent(),
                "a blank line must separate the top comment block from the rest of the file"
            );
        }
        if (!this.comments.isEmpty()) {
            emit.comment(this.comments, this.comments.get(this.comments.size() - 1).line());
            this.clearComments();
        }
        this.closed = true;
    }

    /**
     * Whether the parser is still consuming the meta header (R-6.5.5).
     * @return Flag
     */
    boolean inMetaHeader() {
        return this.meta;
    }

    /**
     * Mark that a meta directive has been parsed — opens the meta
     * header for R-6.5.5 timing.
     */
    void markMeta() {
        this.meta = true;
    }

    /**
     * Close the meta header — called once the first non-meta non-blank
     * line lands.
     */
    void closeMetaHeader() {
        this.meta = false;
    }

    /**
     * Current pending-blank count.
     * @return Count
     */
    int pendingBlanks() {
        return this.blanks;
    }

    /**
     * Increment the pending-blank counter.
     */
    void blank() {
        this.blanks = this.blanks + 1;
        this.trailing = this.trailing + 1;
    }

    /**
     * Reset the pending-blank counter — called when a non-blank line
     * lands.
     */
    void clearBlanks() {
        this.blanks = 0;
        this.trailing = 0;
    }

    /**
     * Current trailing-blank count (only meaningful at EOF).
     * @return Count
     */
    int trailingBlanks() {
        return this.trailing;
    }

    /**
     * Whether currently inside an open text block.
     * @return Flag
     */
    boolean inTextBlock() {
        return this.intext;
    }

    /**
     * Source line where the current text block opened.
     * @return Line
     */
    int textBlockOpenLine() {
        return this.tline;
    }

    /**
     * Mark a text block as opened on the given line.
     * @param line Open line
     */
    void openTextBlock(final int line) {
        this.openTextBlock(line, 0);
    }

    /**
     * Mark a text block as opened with a recorded indent.
     * @param line Open line
     * @param indent Open indent
     */
    void openTextBlock(final int line, final int indent) {
        this.intext = true;
        this.tline = line;
        this.tindent = indent;
        this.tbody.clear();
    }

    /**
     * Mark the current text block as closed.
     */
    void closeTextBlock() {
        this.intext = false;
        this.tline = 0;
        this.tindent = 0;
        this.tbody.clear();
    }

    /**
     * Append a body line to the current text block, stripping the
     * opener's indent prefix where present.
     * @param raw Raw line text
     */
    void appendTextLine(final String raw) {
        final String stripped;
        if (raw.length() < this.tindent
            && raw.chars().allMatch(c -> c == ' ')) {
            stripped = "";
        } else if (raw.length() >= this.tindent
            && raw.substring(0, Math.min(this.tindent, raw.length()))
                .chars().allMatch(c -> c == ' ')) {
            stripped = raw.substring(Math.min(this.tindent, raw.length()));
        } else {
            stripped = raw;
        }
        this.tbody.add(stripped);
    }

    /**
     * The accumulated body lines, in source order.
     * @return Body lines
     */
    List<String> tbody() {
        return Collections.unmodifiableList(this.tbody);
    }

    /**
     * Indent at which the current text block opened.
     * @return Open indent
     */
    int textBlockOpenIndent() {
        return this.tindent;
    }

    /**
     * Pending comment lines, in source order.
     * @return Unmodifiable view of the comment buffer
     */
    List<Span> pendingComments() {
        return Collections.unmodifiableList(this.comments);
    }

    /**
     * Append a comment span to the pending buffer.
     * @param span Comment line
     */
    void addComment(final Span span) {
        this.comments.add(span);
    }

    /**
     * Take a savepoint of the whole program-wide state - R-7.2.
     *
     * <p>The flags and buffers kept here outlive the line that touched
     * them, so a line that seals the header zone in
     * {@link #seal(Emit, Span)} and then throws would leave the file
     * without its top comment block and with every later comment
     * rejected. {@link Eo} takes one of these next to
     * {@link Emit#savepoint()} and hands it back to
     * {@link #restore(Globals)} next to {@link Emit#rollback(Savepoint)}.</p>
     *
     * @return Detached copy of the current state
     */
    Globals savepoint() {
        final Globals saved = new Globals();
        saved.restore(this);
        return saved;
    }

    /**
     * Restore the state from a savepoint, discarding every mutation
     * made since it was taken.
     * @param saved A savepoint taken earlier via {@link #savepoint()}
     */
    void restore(final Globals saved) {
        this.emitted = saved.emitted;
        this.meta = saved.meta;
        this.blanks = saved.blanks;
        this.trailing = saved.trailing;
        this.intext = saved.intext;
        this.tline = saved.tline;
        this.tindent = saved.tindent;
        this.tbody.clear();
        this.tbody.addAll(saved.tbody);
        if (!this.discarded) {
            this.closed = saved.closed;
            this.comments.clear();
            this.comments.addAll(saved.comments);
        }
    }

    /**
     * Drop all pending comments — called once they have been attached or
     * reported as dangling.
     */
    void clearComments() {
        this.comments.clear();
    }

    /**
     * Throw the top comment block away and close the header zone, for
     * good - the block is malformed and no later line may pick it up
     * or report it a second time, not even after a rollback.
     */
    void dropComments() {
        this.comments.clear();
        this.closed = true;
        this.discarded = true;
    }
}
