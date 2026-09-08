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
 * What the packages of a program hand to the objects they are named after.
 *
 * <p>A package is a prefix of a locator, so {@code minus} in the package
 * {@code number} is {@code Φ.number.minus} and belongs to {@code Φ.number}
 * without ever appearing among the children of that formation. Reading the
 * object at the top of every file is what finds those attributes, and it is
 * the only way to find them: nothing inside the formation mentions them.</p>
 *
 * <p>A row is written only when the program forms something under the
 * prefix. A package nobody declares as an object is a prefix and nothing
 * else, and giving it rows would put a type into the table that the program
 * does not have.</p>
 *
 * @since 0.68.0
 */
final class Members {

    /**
     * The formations of the program, the only objects a package can add to.
     */
    private final Collection<XML> made;

    /**
     * The object every file of the program is about.
     */
    private final Collection<XML> roots;

    /**
     * Ctor.
     * @param formations The formations of the program
     * @param tops The object every file is about
     */
    Members(final Collection<XML> formations, final Collection<XML> tops) {
        this.made = formations;
        this.roots = tops;
    }

    /**
     * Write these attributes into the given table.
     * @param rows The table to fill
     */
    void fill(final Tojos rows) {
        final Collection<String> owners = new HashSet<>(0);
        for (final XML formation : this.made) {
            owners.add(new Noted(formation).says("loc"));
        }
        for (final XML root : this.roots) {
            final Noted member = new Noted(root);
            final String type = member.says("loc");
            final String owner = type.substring(0, type.lastIndexOf('.'));
            if (owners.contains(owner)) {
                final String name = member.says("name");
                rows.add(String.join(" ", owner, name))
                    .set("owner", owner)
                    .set("name", name)
                    .set("type", type);
            }
        }
    }
}
