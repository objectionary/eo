/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.yegor256.tojos.Tojos;
import java.util.Collection;
import java.util.HashSet;

/**
 * What every object of a program sits in.
 *
 * <p>An object answers to {@code ρ} for the one it is written inside, and an
 * outer name is lowered into a dispatch of exactly that, so the needs table
 * asks about {@code ρ} all the time. No row ever mentioned it, which left
 * every such question unanswered while the owner was called complete all the
 * same — the one way this table could say "there is no such attribute" about
 * an attribute every object has.</p>
 *
 * <p>Nobody has to be asked for the answer. A locator is the path to where an
 * object is written, so the nearest formation the path runs through is what
 * the object sits in. The object a file is about runs through none, and sits
 * in the root: {@code Φ} describes nothing and is not meant to, since "it sits
 * in the root" and "it sits in nothing" are different answers and only the
 * first is true.</p>
 *
 * @since 0.69.0
 */
final class Parents {

    /**
     * The formations of the program.
     */
    private final Collection<XML> made;

    /**
     * Ctor.
     * @param formations The formations of the program
     */
    Parents(final Collection<XML> formations) {
        this.made = formations;
    }

    /**
     * Write these attributes into the given table.
     * @param rows The table to fill
     */
    void fill(final Tojos rows) {
        final Collection<String> owners = new HashSet<>(0);
        for (final XML formation : this.made) {
            owners.add(formation.xpath("@loc").get(0));
        }
        final Nesting nesting = new Nesting(owners);
        for (final XML formation : this.made) {
            final String owner = formation.xpath("@loc").get(0);
            final String sits = nesting.around(owner);
            final String root;
            if (sits.isEmpty()) {
                root = "Φ";
            } else {
                root = sits;
            }
            rows.add(String.join(" ", owner, "ρ"))
                .set("owner", owner)
                .set("name", "ρ")
                .set("type", root);
        }
    }
}
