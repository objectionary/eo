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
 * What every package of a program holds, the global object {@code Φ} too.
 *
 * <p>A package is an object. The runtime answers {@code Q.io} with a package
 * object when the folder {@code io} has no {@code io.eo} to form one, so
 * {@code Q.io.stdout} is valid with nothing but a {@code [] > stdout} in the
 * package {@code io}. Every prefix of a locator that the program does not form
 * as an object is such a package, and it gets a row of its own, with the next
 * level under it as its attributes: {@code Φ} has {@code io}, {@code Φ.io}
 * has {@code stdout}. The global object is the package at the very top, the
 * one every chain of names off a bare {@code Φ} is read from.</p>
 *
 * <p>A package row is complete. The build parses, probes and pulls until no
 * new object turns up, so every object the program names is in front of the
 * checker, and a name a package lacks is missing for real. A package has no
 * voids and hands back nothing, and having names of its own it is never
 * bare.</p>
 *
 * <p>A prefix the program does form as an object keeps its own row, and its
 * package only adds to it: {@code minus} in the package {@code number} is
 * {@code Φ.number.minus} and belongs to the {@code Φ.number} of
 * {@code number.eo} without appearing among the children of that formation.
 * Reading the object at the top of every file is what finds those attributes,
 * and the only way to find them. An object that is not a formation, such as
 * {@code number > pinf}, gets nothing added, since it has no row to add
 * to.</p>
 *
 * @since 0.76.0
 */
final class Packages {

    /**
     * The locators of the formations of the program.
     */
    private final Collection<String> owners;

    /**
     * The object every file of the program is about.
     */
    private final Collection<XML> roots;

    /**
     * Ctor.
     *
     * @param made The locators of the formations of the program
     * @param tops The object every file is about
     */
    Packages(final Collection<String> made, final Collection<XML> tops) {
        this.owners = made;
        this.roots = tops;
    }

    /**
     * Write every package and what it holds into the given table.
     *
     * @param rows The table to fill
     */
    void fill(final Tojos rows) {
        final Collection<String> objects = new HashSet<>(this.owners);
        for (final XML root : this.roots) {
            objects.add(new Noted(root).says("loc"));
        }
        rows.add("Φ").set("complete", "true");
        for (final XML root : this.roots) {
            this.hang(rows, objects, new Noted(root).says("loc"));
        }
    }

    private void hang(final Tojos rows, final Collection<String> objects, final String member) {
        final String owner = member.substring(0, member.lastIndexOf('.'));
        final boolean folder = !objects.contains(owner);
        if (folder || this.owners.contains(owner)) {
            final String name = member.substring(member.lastIndexOf('.') + 1);
            rows.add(String.join(" ", owner, name))
                .set("owner", owner)
                .set("name", name)
                .set("type", member);
        }
        if (folder && owner.contains(".")) {
            rows.add(owner).set("complete", "true");
            this.hang(rows, objects, owner);
        }
    }
}
