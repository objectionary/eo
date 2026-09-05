/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What every atom comes back with.
 *
 * <p>The body of an atom is written in Java and nothing here can read it, yet
 * the source says what running it gives back: {@code [] > div /Q.number} says
 * a {@code div} is a {@code Φ.number} once it has run. The parser carries that
 * annotation into the XMIR and {@link Provides} keeps it as a column, so the
 * answer is one column away from whoever wants it.</p>
 *
 * <p>Two want it, which is why the reading of it lives here and in neither of
 * them: one walks a chain of copies to its end, where a caller was handed what
 * the atom comes back with and not the atom, and one shows a reader their own
 * file, where the body of an atom is the {@code λ} nobody types. Both are
 * answering the same question and must not answer it apart.</p>
 *
 * <p>An annotation that names a variable rather than an object reaches the
 * column as the void that carries the same letter, {@code Provides} having put
 * it there: {@code [] > recovered /A} comes back with whatever the caller put
 * in, so the column holds the void it was put in, and a caller who filled it
 * with a {@code number} is answered with a {@code number} (#8348).</p>
 *
 * @since 0.71.0
 */
final class Returned {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Ctor.
     * @param provides The provides table, which says what an atom comes back
     *  with
     */
    Returned(final XML provides) {
        this.given = provides;
    }

    /**
     * What every atom comes back with.
     * @return The forma, by the locator of the atom, without the atoms that
     *  declare nothing a reader could go and look at
     */
    Map<String, String> all() {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Xnav type : new Rows(this.given).all()) {
            final String owner = new Noted(type).says("id");
            type.attribute("returns").text().ifPresent(back -> found.put(owner, back));
        }
        return found;
    }

    /**
     * What the body of every atom comes back with.
     * @return The forma, by the locator of the body, which is the {@code λ}
     *  nobody types and which is where a reader hovers to be told
     */
    Map<String, String> bodies() {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, String> atom : this.all().entrySet()) {
            found.put(String.join(".", atom.getKey(), "λ"), atom.getValue());
        }
        return found;
    }
}
