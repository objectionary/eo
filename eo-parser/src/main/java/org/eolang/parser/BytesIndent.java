/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * What is wrong with the indent of one line of a multi-line bytes literal.
 *
 * <p>A continuation is indented with spaces, must not de-indent below the
 * line that opened the literal, must not sit on an odd indent (R-2.2.1), and
 * must not jump more than one level deeper than the line above it, which is
 * the same rule every other block line obeys.</p>
 *
 * @since 0.1
 */
final class BytesIndent {

    /**
     * The continuation line.
     */
    private final Span line;

    /**
     * Indent of the line that opened the literal.
     */
    private final int head;

    /**
     * Indent of the line right above this one.
     */
    private final int above;

    /**
     * Ctor.
     * @param span The continuation line
     * @param opener Indent of the line that opened the literal
     * @param previous Indent of the line right above this one
     */
    BytesIndent(final Span span, final int opener, final int previous) {
        this.line = span;
        this.head = opener;
        this.above = previous;
    }

    /**
     * Report what is wrong with this line, if anything is.
     * @param emit Where the complaint goes
     * @return True when the line was reported
     */
    boolean reported(final Emit emit) {
        final String complaint = this.complaint();
        final boolean wrong = !complaint.isEmpty();
        if (wrong) {
            emit.error(this.line.line(), 0, complaint);
        }
        return wrong;
    }

    private String complaint() {
        final String found;
        if (this.line.blank()) {
            found = "";
        } else if (this.line.tab()) {
            found = Eo.TAB;
        } else if (this.line.alien()) {
            found = Eo.ALIEN;
        } else if (this.line.indent() < this.head) {
            found = "multi-line bytes continuation must not de-indent";
        } else if (this.line.indent() % 2 == 1) {
            found = "unexpected odd indent";
        } else if (this.line.indent() > this.above + 2) {
            found = "indent increased by more than one level";
        } else {
            found = "";
        }
        return found;
    }
}
