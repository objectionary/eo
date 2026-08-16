/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * What a program asks of every void, gathered onto the void that is asked.
 *
 * <p>{@link Provides} writes a void down as an empty attribute and
 * {@link Needs} records what somebody wants from it, but the two never
 * meet: finding out that a demand landed on a void means resolving every
 * dispatch in the program again. That is what this clue does, once
 * {@link Resolved} has closed the links. A demand whose receiver resolves
 * to a void, or to a name rooted at one, is gathered onto that void as a
 * {@code demand} child of its row:</p>
 *
 * <pre> &lt;attr name="as-bytes" type="Φ.number.as-bytes" void="true"&gt;
 *   &lt;demand name="size" type="Φ.number.as-bytes.size"/&gt;
 * &lt;/attr&gt;</pre>
 *
 * <p>A demand of a deeper variable nests under the demand that named it,
 * so {@code x.next.foo} reads as a {@code foo} under a {@code next}. A
 * demand whose receiver does not resolve to a void is left where it is —
 * the tables record what a program asks, not what an object turns out to
 * be.</p>
 *
 * @since 0.67.0
 */
public final class Demands implements Clue {

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        final Path provided = tables.resolve("provides.xml");
        final Path needed = tables.resolve("needs.xml");
        final Path linked = tables.resolve("links.xml");
        if (Files.exists(provided) && Files.exists(needed) && Files.exists(linked)) {
            final XML gives = new XMLDocument(provided);
            if (!gives.xpath("//attr[@void='true']/@type").isEmpty()) {
                Demands.amend(provided, gives, needed, linked);
            }
        }
    }

    /**
     * Gather the demands onto the voids and write the provides table again.
     * @param provided The provides document
     * @param gives The provides document, for the base of the rewrite
     * @param needed The needs document
     * @param linked The links document
     * @throws IOException If the table cannot be written
     */
    private static void amend(
        final Path provided, final XML gives, final Path needed, final Path linked
    ) throws IOException {
        final Map<String, List<Demands.Demand>> roots = Demands.gather(
            new XMLDocument(needed),
            gives.xpath("//attr[@void='true']/@type"),
            new Ends(Demands.pairs(new XMLDocument(linked))).names()
        );
        if (!roots.isEmpty()) {
            final Directives dirs = new Directives();
            for (final Map.Entry<String, List<Demands.Demand>> root : roots.entrySet()) {
                dirs.xpath(String.format("//attr[@type='%s']", root.getKey()));
                for (final Demands.Demand demand : root.getValue()) {
                    Demands.build(dirs, demand);
                }
            }
            Files.write(
                provided,
                new XMLDocument(
                    new Xembler(dirs).applyQuietly(gives.node())
                ).toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }

    /**
     * The resolved pairs of the links table: every name against the one it
     * is a copy of.
     * @param links The links document
     * @return The pairs
     */
    private static Map<String, String> pairs(final XML links) {
        final Map<String, String> pairs = new HashMap<>(0);
        for (final XML type : links.nodes("/links/type")) {
            for (final String loc : type.xpath("ref/@loc")) {
                pairs.put(type.xpath("@id").get(0), loc);
            }
        }
        return pairs;
    }

    /**
     * Every demand of the program whose receiver resolves, by where the
     * receiver resolves to.
     * @param needs The needs document
     * @param names Where every locator resolves to
     * @return The demands, by the resolved receiver
     */
    private static Map<String, List<Demands.Demand>> asked(
        final XML needs, final Map<String, String> names
    ) {
        final Map<String, List<Demands.Demand>> where = new LinkedHashMap<>(0);
        for (final XML type : needs.nodes("/needs/type")) {
            final String owner = type.xpath("@id").get(0);
            for (final XML attr : type.nodes("attr")) {
                final String made = names.get(attr.xpath("@type").get(0));
                final String asked = names.get(owner);
                if (made != null && asked != null) {
                    where.computeIfAbsent(asked, key -> new ArrayList<>(1))
                        .add(new Demands.Demand(attr.xpath("@name").get(0), made));
                }
            }
        }
        return where;
    }

    /**
     * The demand every locator is produced by, for attaching nested demands.
     * @param where The demands, by the resolved receiver
     * @return The demands, by what they resolve to
     */
    private static Map<String, Demands.Demand> nodes(
        final Map<String, List<Demands.Demand>> where
    ) {
        final Map<String, Demands.Demand> nodes = new HashMap<>(0);
        for (final List<Demands.Demand> group : where.values()) {
            for (final Demands.Demand demand : group) {
                nodes.put(demand.type, demand);
            }
        }
        return nodes;
    }

    /**
     * Every demand of the program that is asked of a void, gathered onto
     * the void, with deeper demands nested under the one that named them.
     * @param needs The needs document
     * @param voids The locators of every void
     * @param names Where every locator resolves to
     * @return The root demands, by the void they are asked of
     */
    private static Map<String, List<Demands.Demand>> gather(
        final XML needs, final Collection<String> voids, final Map<String, String> names
    ) {
        final Map<String, List<Demands.Demand>> where = Demands.asked(needs, names);
        final Map<String, Demands.Demand> nodes = Demands.nodes(where);
        final Map<String, List<Demands.Demand>> roots = new LinkedHashMap<>(0);
        for (final Map.Entry<String, List<Demands.Demand>> group : where.entrySet()) {
            final String voided = Demands.root(group.getKey(), voids);
            if (voided == null) {
                continue;
            }
            final Demands.Demand parent = nodes.get(group.getKey());
            if (parent == null) {
                roots.computeIfAbsent(voided, key -> new ArrayList<>(1))
                    .addAll(group.getValue());
            } else {
                parent.children.addAll(group.getValue());
            }
        }
        return roots;
    }

    /**
     * The void a locator is rooted at, or {@code null} when it is not.
     * The most specific void wins, since one locator can sit inside several.
     * @param loc The locator
     * @param voids The locators of every void
     * @return The void, or {@code null}
     */
    private static String root(final String loc, final Collection<String> voids) {
        return voids.stream()
            .filter(voided -> loc.equals(voided) || loc.startsWith(voided.concat(".")))
            .max(Comparator.comparingInt(String::length))
            .orElse(null);
    }

    /**
     * Append a demand and its nested demands to the directives, cursor on
     * the parent when called and on it again when it returns.
     * @param dirs The directives
     * @param demand The demand to append
     */
    private static void build(final Directives dirs, final Demands.Demand demand) {
        dirs.add("demand").attr("name", demand.name).attr("type", demand.type);
        for (final Demands.Demand child : demand.children) {
            Demands.build(dirs, child);
        }
        dirs.up();
    }

    /**
     * One thing a program asks of a void: a name, what asking for it turns
     * out to be, and the demands asked of that in turn.
     * @since 0.67.0
     */
    private static final class Demand {

        /**
         * The name asked.
         */
        private final String name;

        /**
         * What the name resolves to.
         */
        private final String type;

        /**
         * The demands asked of this one in turn.
         */
        private final List<Demands.Demand> children;

        /**
         * Ctor.
         * @param name The name asked
         * @param type What the name resolves to
         */
        private Demand(final String name, final String type) {
            this.name = name;
            this.type = type;
            this.children = new ArrayList<>(0);
        }
    }
}
