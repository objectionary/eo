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
 * <p>The types are read from the tables of {@code eo:inference},
 * {@code facts.xml} and {@code answers.xml}. A type that goes in is what
 * one void of the object holds, found where {@link Planting} finds it, in
 * {@code entries.xsl}: the annotation of the void in {@code facts.xml}, the
 * formation a declared {@code ρ} is written in, or the only object the row
 * of the void in {@code answers.xml} names. The output follows
 * {@code eo:returns} in {@code _returns.xsl}: the objects the row of the
 * body names in {@code answers.xml}, each counted as the type an atom
 * returns in {@code facts.xml}, as what a formation reduces to other than
 * itself, or as itself, in that order. A row says a termination, an open
 * void or a failing read next to the objects, and those are left out.</p>
 *
 * <p>This is used only to tell the reader of the log what the atoms take
 * and give: the name of the object, the types of its voids in brackets,
 * an arrow, and the type of its body. So a table that is
 * missing, or that says nothing about a type, does not stop the build: the
 * type is shown as {@code ?}.</p>
 *
 * @since 0.64.0
 */
final class Signatures {

    /**
     * The table {@code facts.xml}, with the voids, their annotations and
     * what the atoms return.
     */
    private final Unchecked<XML> facts;

    /**
     * The table {@code answers.xml}, with what every place is.
     */
    private final Unchecked<XML> answers;

    /**
     * Ctor.
     *
     * @param tables The directory with the tables of {@code eo:inference}
     */
    Signatures(final Path tables) {
        this(
            new Sticky<>(() -> Signatures.table(tables.resolve("facts.xml"))),
            new Sticky<>(() -> Signatures.table(tables.resolve("answers.xml")))
        );
    }

    /**
     * Ctor.
     *
     * @param fcts The table with the voids and what the atoms return
     * @param said The table with what every place is
     */
    Signatures(final Scalar<XML> fcts, final Scalar<XML> said) {
        this.facts = new Unchecked<>(fcts);
        this.answers = new Unchecked<>(said);
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
        for (final XML attr : this.facts.value().nodes(
            String.format("/facts/objects/type[@id='%s'][1]/attr[@void='true']", loc)
        )) {
            final String name = attr.xpath("@name").get(0);
            final List<String> held = new ArrayList<>(attr.xpath("@holds"));
            if ("ρ".equals(name)) {
                held.addAll(attr.xpath("@rho"));
            } else {
                held.addAll(this.one(String.format("%s.%s", loc, name)));
            }
            if (held.isEmpty()) {
                ins.add("?");
            } else {
                ins.add(held.get(0).replaceAll("\\?$", ""));
            }
        }
        final Collection<String> outs = new LinkedHashSet<>(0);
        for (final String name : this.names(String.format("%s.φ", loc), "")) {
            outs.addAll(this.behaves(name));
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

    private List<String> one(final String place) {
        final List<String> found = new ArrayList<>(this.names(place, ""));
        if (found.size() != 1 || !this.answers.value().nodes(
            String.format(
                "%1$s/*[not(self::is or self::set or self::reduced or %2$s)] | %1$s/set/*[not(self::is or %2$s)]",
                String.format("/answers/o[@loc='%s']", place),
                "self::terminator or self::open or self::fails"
            )
        ).isEmpty()) {
            found.clear();
        }
        return found;
    }

    private Collection<String> behaves(final String name) {
        final Collection<String> found = new LinkedHashSet<>(
            this.facts.value().xpath(
                String.format("/facts/objects/type[@id='%s'][1]/@returns", name)
            )
        );
        if (found.isEmpty()) {
            for (final String other : this.names(name, "/reduced")) {
                if (!other.equals(name)) {
                    found.add(other);
                }
            }
        }
        if (found.isEmpty()) {
            found.add(name);
        }
        return found;
    }

    private Collection<String> names(final String place, final String part) {
        return new LinkedHashSet<>(
            this.answers.value().xpath(
                String.format(
                    "/answers/o[@loc='%1$s']%2$s/is/@loc | /answers/o[@loc='%1$s']%2$s/set/is/@loc",
                    place, part
                )
            )
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
