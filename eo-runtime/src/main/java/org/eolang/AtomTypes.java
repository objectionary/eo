/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Map;

/**
 * Declared return types of all atoms, keyed by the atom's forma.
 *
 * <p>The table is produced at build time from the {@code /Q.foo} return-type
 * suffixes that atoms declare in EO (the {@code atom} attribute of their
 * lambda in XMIR). It answers, for an atom of a given forma, the forma its
 * computed object is required to have. An atom that declares no type maps to
 * an empty string.</p>
 *
 * @since 0.57
 * @todo #5189:90min The table is not produced at build time yet; it is read
 *  from a hand-written {@code atoms.csv} resource by {@link PhDefault}. It must
 *  be generated automatically from the {@code atom} attribute of every lambda
 *  in the XMIR, so it never drifts from the EO sources, after which the
 *  hand-written {@code atoms.csv} can be removed.
 */
public final class AtomTypes {

    /**
     * Forma of the atom mapped to the forma of its computed object.
     */
    private final Map<String, String> table;

    /**
     * Ctor.
     * @param types Forma of the atom to forma of its computed object
     */
    public AtomTypes(final Map<String, String> types) {
        this.table = types;
    }

    /**
     * The forma declared for the object computed by the atom of this forma.
     * @param forma Forma of the atom
     * @return Declared forma, or empty string when nothing is declared
     */
    public String declared(final String forma) {
        return this.table.getOrDefault(forma, "");
    }
}
