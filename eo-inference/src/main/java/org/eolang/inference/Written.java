/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * An object as the source wrote it, with what we found out about it.
 *
 * <p>The tables know objects by locator and a reader knows them by where they
 * are on the page, so something has to hold both at once. This does: the
 * locator, the column it was written at, the name it goes by, and the answer
 * the tables gave for it.</p>
 *
 * <p>Some names are written nowhere. The object a dispatch is taken from
 * answers to {@code ρ} without that name appearing anywhere in the line, and
 * a formation that delegates has a {@code φ} that may be spelled {@code @} or
 * not spelled at all. Such an object still has a place, since it was found at
 * some line and column, and still deserves saying out loud, so the name it
 * goes by is what a reader would call it rather than the glyph.</p>
 *
 * @since 0.70.0
 */
final class Written {

    /**
     * The locator of the object.
     */
    private final String locator;

    /**
     * The column it was written at, counted from nought.
     */
    private final int column;

    /**
     * The name it goes by.
     */
    private final String called;

    /**
     * What the tables answered for it.
     */
    private final Answer told;

    /**
     * Ctor.
     *
     * @param loc The locator of the object
     * @param place The column it was written at, counted from nought
     * @param name The name it goes by, empty when it has none
     * @param answer What the tables answered for it
     */
    Written(final String loc, final int place, final String name, final Answer answer) {
        this.locator = loc;
        this.column = place;
        this.called = name;
        this.told = answer;
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
     * @return The column, counted from nought
     */
    int at() {
        return this.column;
    }

    /**
     * What a reader should call it.
     *
     * <p>A name the source never wrote is said in words instead of in its
     * glyph, since {@code ρ} on a page tells a reader nothing they did not
     * already have to know.</p>
     *
     * @return The label
     */
    String label() {
        final String plain;
        if (this.called.isEmpty()) {
            plain = this.locator.substring(this.locator.lastIndexOf('.') + 1);
        } else {
            plain = this.called;
        }
        final String found;
        if ("@".equals(plain) || "φ".equals(plain)) {
            found = "what it decorates to (φ)";
        } else if ("λ".equals(plain)) {
            found = "what the atom comes back with (λ)";
        } else if ("ρ".equals(plain)) {
            found = "what it is dispatched on (ρ)";
        } else if (plain.startsWith("α")) {
            found = "argument ".concat(plain.substring(1));
        } else {
            found = plain;
        }
        return found;
    }

    /**
     * What the tables answered for it.
     *
     * @return The answer
     */
    Answer answer() {
        return this.told;
    }

    /**
     * The same object, written at another column.
     *
     * @param place The column
     * @return The object
     */
    Written moved(final int place) {
        return new Written(this.locator, place, this.called, this.told);
    }
}
