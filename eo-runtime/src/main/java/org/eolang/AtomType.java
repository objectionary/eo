/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The type an atom declares for the object it computes, as the table of
 * atoms keeps it.
 *
 * <p>The type is the {@code /Q.foo} suffix of the atom in EO, with every
 * {@code Q} turned into {@code Φ}. It is one option, or a union of two split
 * by a {@code |} that stands outside every bracket, such as
 * {@code Φ.true|Φ.false}. An option is a type variable, {@code A} to
 * {@code F}, or a forma with the types of its voids in brackets right after
 * it, such as {@code Φ.posix.status-return(Φ.number,Φ.string)}. A computed
 * object fits the type when its forma is the forma either option starts
 * with, the name before the brackets. The brackets are not checked, since
 * that would dataize the voids of the computed object. A type variable and
 * an empty type fit any object.</p>
 *
 * @since 0.76.0
 */
final class AtomType {

    /**
     * The type, as the table of atoms keeps it.
     */
    private final String type;

    /**
     * Ctor.
     *
     * @param declared The type, as the table of atoms keeps it
     */
    AtomType(final String declared) {
        this.type = declared;
    }

    /**
     * Whether an object of this forma fits the type.
     *
     * @param forma The forma of the computed object
     * @return TRUE if it fits
     */
    boolean admits(final String forma) {
        final StringBuilder outside = new StringBuilder(this.type.length());
        int depth = 0;
        for (final char glyph : this.type.toCharArray()) {
            if (glyph == '(') {
                depth += 1;
            } else if (glyph == ')') {
                depth -= 1;
            } else if (depth == 0) {
                outside.append(glyph);
            }
        }
        boolean fits = this.type.isEmpty();
        for (final String head : outside.toString().split("\\|", -1)) {
            fits = fits || head.matches("[A-F]") || head.equals(forma);
        }
        return fits;
    }
}
