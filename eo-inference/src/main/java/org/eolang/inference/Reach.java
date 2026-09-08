/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * How far the thing written at a place in a line reaches.
 *
 * <p>A locator says which line an object was written on and which column it
 * starts at, and says nothing about where it ends. To mark it on a page the
 * end has to be found, and the only place it can be found is the line itself,
 * so the text is read forward from the column until the thing written there
 * runs out.</p>
 *
 * <p>What counts as running out depends on what was started. A name runs to
 * the first character that cannot be part of one. A literal number carries its
 * digits, its hex letters, its sign and one decimal point. A string runs to
 * its closing quote, and a backslash inside it takes the next character with
 * it, so that {@code "a\"b"} is one string and not two. A dispatch takes its
 * dot along with what follows it, be that a name or the {@code ^} of
 * {@code .^}, since a dot alone marks nothing a reader can read. Anything else
 * is one character, which is the honest answer for a glyph nobody taught this
 * about.</p>
 *
 * @since 0.70.0
 */
final class Reach {

    /**
     * The line, as the source wrote it.
     */
    private final String line;

    /**
     * Ctor.
     * @param text The line, as the source wrote it
     */
    Reach(final String text) {
        this.line = text;
    }

    /**
     * How many characters the thing at this column covers.
     * @param column The column it starts at, counted from nought
     * @return The characters, never fewer than none
     */
    int from(final int column) {
        final int found;
        if (column < 0 || column >= this.line.length()) {
            found = 0;
        } else {
            found = this.spanned(column);
        }
        return found;
    }

    private int spanned(final int column) {
        final char head = this.line.charAt(column);
        final int found;
        if (head == '"') {
            found = this.quoted(column);
        } else if (Character.isDigit(head)
            || head == '-' && Character.isDigit(this.after(column))) {
            found = this.numbered(column);
        } else if (Character.isLetterOrDigit(head) || head == '_') {
            found = this.named(column);
        } else if (head == '.'
            && (Character.isLetterOrDigit(this.after(column)) || this.after(column) == '^')) {
            found = 1 + this.from(column + 1);
        } else {
            found = 1;
        }
        return found;
    }

    private int quoted(final int column) {
        int cursor = column + 1;
        int found = this.line.length() - column;
        while (cursor < this.line.length()) {
            if (this.line.charAt(cursor) == '\\') {
                cursor = cursor + 2;
                continue;
            }
            if (this.line.charAt(cursor) == '"') {
                found = cursor + 1 - column;
                break;
            }
            cursor = cursor + 1;
        }
        return found;
    }

    private int numbered(final int column) {
        int cursor = column + 1;
        while (cursor < this.line.length()) {
            final char here = this.line.charAt(cursor);
            if (Character.digit(here, 16) >= 0 || here == '-') {
                cursor = cursor + 1;
            } else if (here == '.' && Character.isDigit(this.after(cursor))) {
                cursor = cursor + 2;
            } else {
                break;
            }
        }
        return cursor - column;
    }

    private int named(final int column) {
        int cursor = column;
        while (cursor < this.line.length()
            && (Character.isLetterOrDigit(this.line.charAt(cursor))
                || this.line.charAt(cursor) == '_'
                || this.line.charAt(cursor) == '-')) {
            cursor = cursor + 1;
        }
        return cursor - column;
    }

    private char after(final int column) {
        final char found;
        if (column + 1 < this.line.length()) {
            found = this.line.charAt(column + 1);
        } else {
            found = ' ';
        }
        return found;
    }
}
