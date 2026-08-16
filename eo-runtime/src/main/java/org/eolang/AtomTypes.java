/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Collections;
import java.util.Map;
import java.util.stream.Collectors;

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
 */
public final class AtomTypes {

    /**
     * Forma of the atom mapped to the forma of its computed object.
     */
    private final Map<String, String> table;

    /**
     * Ctor that loads the table generated next to the given class.
     * @param owner Class whose package holds the generated {@code atoms.csv}
     */
    public AtomTypes(final Class<?> owner) {
        this(AtomTypes.loaded(owner));
    }

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

    /**
     * Load the declared return types of all atoms from the generated table.
     * @param owner Class whose package holds the generated {@code atoms.csv}
     * @return Forma of the atom to forma of its computed object, empty when the table is absent
     */
    private static Map<String, String> loaded(final Class<?> owner) {
        final Map<String, String> table;
        final InputStream source = owner.getResourceAsStream("atoms.csv");
        if (source == null) {
            table = Collections.emptyMap();
        } else {
            try (
                BufferedReader lines = new BufferedReader(
                    new InputStreamReader(source, StandardCharsets.UTF_8)
                )
            ) {
                table = lines.lines().filter(line -> line.contains(",")).collect(
                    Collectors.toMap(
                        line -> line.substring(0, line.indexOf(',')),
                        line -> line.substring(line.indexOf(',') + 1)
                    )
                );
            } catch (final IOException ex) {
                throw new ExFailure("Failed to read the atom types table", ex);
            }
        }
        return table;
    }
}
