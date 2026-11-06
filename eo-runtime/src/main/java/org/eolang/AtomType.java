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
 * {@code Q} turned into {@code Φ}. It is a type variable, {@code A} to
 * {@code F}, or a forma. When EO writes the type as a tree, with the types
 * of its voids in brackets, such as
 * {@code Φ.posix.status-return(Φ.number,Φ.string)}, the table keeps only
 * the name before the brackets, since checking the voids would dataize
 * them. A computed object fits the type when its forma equals it. A type
 * variable and an empty type fit any object.</p>
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
        return this.type.isEmpty() || this.type.matches("[A-F]") || this.type.equals(forma);
    }
}
