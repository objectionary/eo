/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * An object as the source wrote it: where it starts and what it is called.
 *
 * <p>The table knows objects by locator and a reader knows them by where they
 * are on the page, so something has to hold both at once. This does: the
 * locator, the column it was written at, its name and its base.</p>
 *
 * <p>Several objects often start at one column, a read and the object it is
 * read off, or a number and the bytes inside it, and a mark over the word
 * there tells only one of them: the one the source spells. That is the one
 * whose base is the word ({@code T}, {@code ^}, {@code Q} and {@code $} are
 * <code>&#x22A5;</code>, {@code ρ}, {@code Φ} and {@code ξ} there), else the one with
 * that name, else the outermost, whose locator is the shortest.</p>
 *
 * @since 0.70.0
 */
final class Written {

    /**
     * The locator of the object.
     */
    private final String locator;

    /**
     * The column it was written at, counted from nought in UTF-16 units.
     */
    private final int column;

    /**
     * Its name, empty when it has none.
     */
    private final String called;

    /**
     * Its base, empty when it has none.
     */
    private final String base;

    /**
     * Ctor.
     *
     * @param loc The locator of the object
     * @param place The column it was written at, counted from nought
     * @param name Its name, empty when it has none
     * @param copied Its base, empty when it has none
     */
    Written(final String loc, final int place, final String name, final String copied) {
        this.locator = loc;
        this.column = place;
        this.called = name;
        this.base = copied;
    }

    /**
     * The locator of the object.
     *
     * @return The locator
     */
    String loc() {
        return this.locator;
    }

    /**
     * The column it was written at.
     *
     * @return The column, counted from nought in UTF-16 units
     */
    int at() {
        return this.column;
    }

    /**
     * How many receivers deep it sits, which is how many times its locator
     * steps into a {@code ρ}.
     *
     * @return The steps
     */
    int hops() {
        int found = 0;
        int spot = this.locator.indexOf(".ρ");
        while (spot >= 0) {
            found = found + 1;
            spot = this.locator.indexOf(".ρ", spot + 2);
        }
        return found;
    }

    /**
     * Whether it is what the other object is read off.
     *
     * @param read The other object
     * @return TRUE when its locator is the other's with {@code .ρ} after it
     */
    boolean receives(final Written read) {
        return this.locator.equals(read.locator.concat(".ρ"));
    }

    /**
     * How far it is from being the object the word on the page spells.
     *
     * @param word The word the mark is drawn over, its leading dots dropped
     *  and {@code T}, {@code ^}, {@code Q} and {@code $} read as the names
     *  they stand for
     * @return Nought when its base is the word, one when its name is, two
     *  otherwise
     */
    int spelling(final String word) {
        final int found;
        if (this.base.replaceFirst("^\\.+", "").equals(word)) {
            found = 0;
        } else if (this.called.equals(word)) {
            found = 1;
        } else {
            found = 2;
        }
        return found;
    }

    /**
     * How deep its locator goes, counted in dots.
     *
     * @return The dots
     */
    int depth() {
        return (int) this.locator.chars().filter(glyph -> glyph == '.').count();
    }
}
