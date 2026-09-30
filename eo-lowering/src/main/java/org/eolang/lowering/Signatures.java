/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * The types that go into an object and the type that comes out of it.
 *
 * <p>The types are read from the tables of {@code eo:inference}. A type
 * that goes in is the type of one void of the object, and it is found in
 * {@code provides.xml}, in the same place where {@link Planting} finds
 * it. The type that comes out is the type of the {@code φ} of the object,
 * which is found in {@code links.xml}. When that {@code φ} is an atom, the
 * type is the one that {@code atoms.xml} gives to that atom.</p>
 *
 * <p>This is used only to tell the reader of the log what the atoms take
 * and give: the name of the object, the types of its voids in brackets,
 * an arrow, and the type of its body. So a table that is
 * missing, or that says nothing about a type, does not stop the build: the
 * type is shown as {@code ?}.</p>
 *
 * @since 0.74.0
 */
final class Signatures {

    /**
     * The table {@code provides.xml}, with the types of the voids.
     */
    private final Unchecked<XML> provides;

    /**
     * The table {@code links.xml}, with the types of the bodies.
     */
    private final Unchecked<XML> links;

    /**
     * The table {@code atoms.xml}, with the types of the atoms.
     */
    private final Unchecked<XML> atoms;

    /**
     * Ctor.
     *
     * @param tables The directory with the tables of {@code eo:inference}
     */
    Signatures(final Path tables) {
        this(
            new Sticky<>(() -> Signatures.table(tables.resolve("provides.xml"))),
            new Sticky<>(() -> Signatures.table(tables.resolve("links.xml"))),
            new Sticky<>(() -> Signatures.table(tables.resolve("atoms.xml")))
        );
    }

    /**
     * Ctor.
     *
     * @param prv The table with the types of the voids
     * @param lnk The table with the types of the bodies
     * @param atm The table with the types of the atoms
     */
    Signatures(final Scalar<XML> prv, final Scalar<XML> lnk, final Scalar<XML> atm) {
        this.provides = new Unchecked<>(prv);
        this.links = new Unchecked<>(lnk);
        this.atoms = new Unchecked<>(atm);
    }

    /**
     * The signature of the object with this locator: its name, the types
     * of its voids in brackets, an arrow, and the type of its body.
     *
     * @param loc The locator of the object, such as {@code Φ.i16.as-i32}
     * @return The name of the object, the types of its voids, and its type
     */
    String of(final String loc) {
        final Collection<String> ins = new ArrayList<>(0);
        for (final XML attr : this.provides.value().nodes(
            String.format("/provides/type[@id='%s']/attr[@void='true']", loc)
        )) {
            final List<String> held = attr.xpath("@holds | @settled");
            if (held.isEmpty()) {
                ins.add("?");
            } else {
                ins.add(held.get(0).replaceAll("\\?$", ""));
            }
        }
        final Collection<String> outs = new LinkedHashSet<>(0);
        for (final String ref : this.links.value().xpath(
            String.format("/links/type[@id='%s.φ']/ref/@loc", loc)
        )) {
            final List<String> forma = this.atoms.value().xpath(
                String.format("/atoms/atom[@loc='%s']/@forma", ref)
            );
            if (forma.isEmpty()) {
                outs.add(ref);
            } else {
                outs.add(forma.get(0));
            }
        }
        if (outs.isEmpty()) {
            outs.add("?");
        }
        return String.format(
            "%s(%s)→ %s",
            loc.substring(loc.lastIndexOf('.') + 1),
            String.join(", ", ins),
            String.join(" | ", outs)
        );
    }

    private static XML table(final Path file) throws IOException {
        final XML xml;
        if (Files.exists(file)) {
            xml = new XMLDocument(file);
        } else {
            xml = new XMLDocument("<none/>");
        }
        return xml;
    }
}
