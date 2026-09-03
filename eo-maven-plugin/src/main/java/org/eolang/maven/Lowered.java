/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;
import org.eolang.lowering.JavaAtom;
import org.eolang.lowering.Phino;
import org.eolang.lowering.Protocol;
import org.eolang.lowering.Reduction;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Rewrite the pure formations of one XMIR into synthetic atoms.
 *
 * <p>A formation qualifies when {@code purify.xsl} stamps it pure, it is
 * a named direct attribute of a top-level object, its body is voids plus
 * one {@code φ} and nothing else — deleting the body must not change the
 * object's interface — it neither declares nor reads {@code ρ}, and every
 * void is witnessed in the tables of {@code eo:inference} as a number or
 * as bytes, so a symbolic carrier can stand for it. Such a formation is
 * reduced into a protocol, the protocol is rendered into a Java body, the
 * body goes into a sidecar file named by its own digest, and the
 * formation keeps only its voids, the digest, and a {@code λ} marker —
 * the shape {@code lowered.xsl} later renders into an atom class.
 * Whatever refuses along the way — an unwitnessed void, an operation
 * outside the tables, a body that needs no computation — leaves the
 * formation as written, the way {@code Lowering} treats every
 * refusal.</p>
 *
 * @since 0.76.0
 */
final class Lowered {

    /**
     * The binary that dataizes.
     */
    private final Phino phino;

    /**
     * The train that stamps formations pure.
     */
    private final Xsline purity;

    /**
     * The witnessed data formas, by the locator of each void.
     */
    private final Unchecked<Map<String, String>> formas;

    /**
     * The directory for the sidecar bodies.
     */
    private final Path atoms;

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param tables The directory with the tables of {@link MjInference}
     * @param home The directory for the sidecar bodies
     */
    Lowered(final Phino exe, final Path tables, final Path home) {
        this(
            exe,
            new Xsline(
                new TrDefault<Shift>().with(
                    new StPure("/org/eolang/maven/transpile/purify.xsl", tables)
                )
            ),
            new Unchecked<>(
                new Synced<>(
                    new Sticky<>(() -> Lowered.witnessed(tables.resolve("provides.xml")))
                )
            ),
            home
        );
    }

    /**
     * Ctor.
     * @param exe The binary that dataizes
     * @param train The train that stamps formations pure
     * @param table The witnessed data formas, by the locator of each void
     * @param home The directory for the sidecar bodies
     */
    Lowered(final Phino exe, final Xsline train,
        final Unchecked<Map<String, String>> table, final Path home) {
        this.phino = exe;
        this.purity = train;
        this.formas = table;
        this.atoms = home;
    }

    /**
     * Rewrite every qualifying formation of the document, in place.
     * @param doc The XMIR to rewrite
     * @return How many formations became atoms
     * @throws IOException If a sidecar cannot be written
     */
    int rewrite(final XMLDocument doc) throws IOException {
        final Collection<Xnav> found = Lowered.candidates(doc);
        int count = 0;
        if (!found.isEmpty() && !this.formas.value().isEmpty()) {
            final Set<String> pure = new HashSet<>(
                this.purity.pass(doc).xpath("//o[@pure='true'][not(@base)]/@loc")
            );
            for (final Xnav node : found) {
                final String place = node.attribute("loc").text().orElse("");
                if (pure.contains(place) && this.lowered(node, place)) {
                    ++count;
                }
            }
        }
        return count;
    }

    private boolean lowered(final Xnav node, final String place) throws IOException {
        final Map<String, String> inputs = this.voids(node, place);
        final List<Xnav> bodies = Lowered.bodies(node);
        boolean done = false;
        if (!inputs.isEmpty() && bodies.size() == 1
            && Lowered.kids(node).size() == inputs.size() + 1) {
            done = this.spliced(node, bodies.get(0), inputs);
        }
        return done;
    }

    private boolean spliced(final Xnav node, final Xnav body,
        final Map<String, String> inputs) throws IOException {
        String text = "";
        String carrier = "";
        try {
            final Protocol protocol = new Reduction(this.phino, body, inputs, 8).protocol();
            if (!protocol.moves().isEmpty()) {
                text = new JavaAtom(protocol, inputs).text();
                carrier = protocol.carrier();
            }
        } catch (final IllegalStateException | IOException ex) {
            Logger.debug(this, "A formation stays unlowered: %s", ex.getMessage());
        }
        final boolean done = !carrier.isEmpty();
        if (done) {
            final String digest = new Digest(text).hex();
            new Saved(text, this.atoms.resolve(String.format("%s.java", digest))).value();
            new Xembler(
                new Directives()
                    .attr("pure", "true")
                    .attr("lowered", digest)
                    .xpath("o[@name='φ']").remove()
                    .add("o")
                    .attr("name", "λ")
                    .attr("atom", String.format("Φ.%s", carrier))
            ).applyQuietly(node.node());
            for (final Xnav kid : Lowered.kids(node)) {
                final String name = kid.attribute("name").text().orElse("");
                if (inputs.containsKey(name)) {
                    new Xembler(
                        new Directives().attr(
                            "type", String.format("Φ.%s", inputs.get(name))
                        )
                    ).applyQuietly(kid.node());
                }
            }
        }
        return done;
    }

    private Map<String, String> voids(final Xnav node, final String place) {
        final Map<String, String> out = new LinkedHashMap<>();
        for (final Xnav kid : Lowered.kids(node)) {
            if (!"∅".equals(kid.attribute("base").text().orElse(""))) {
                continue;
            }
            final String name = kid.attribute("name").text().orElse("");
            final String forma = this.formas.value().getOrDefault(
                String.format("%s.%s", place, name), ""
            );
            if ("ρ".equals(name) || forma.isEmpty()) {
                out.clear();
                break;
            }
            out.put(name, forma);
        }
        return out;
    }

    private static Collection<Xnav> candidates(final XMLDocument doc) {
        final Collection<Xnav> out = new ArrayList<>(0);
        for (final Xnav top : Lowered.kids(new Xnav(doc.inner()).element("object"))) {
            if (top.attribute("base").text().isPresent()) {
                continue;
            }
            for (final Xnav kid : Lowered.kids(top)) {
                final String name = kid.attribute("name").text().orElse("");
                if (kid.attribute("base").text().isEmpty()
                    && !name.isEmpty() && !"λ".equals(name)) {
                    out.add(kid);
                }
            }
        }
        return out;
    }

    private static List<Xnav> bodies(final Xnav node) {
        return Lowered.kids(node).stream()
            .filter(kid -> "φ".equals(kid.attribute("name").text().orElse("")))
            .filter(kid -> !kid.attribute("base").text().orElse("").isEmpty())
            .filter(kid -> !"∅".equals(kid.attribute("base").text().orElse("")))
            .collect(Collectors.toList());
    }

    private static Map<String, String> witnessed(final Path table) throws IOException {
        final Map<String, String> out = new HashMap<>(0);
        if (Files.exists(table)) {
            for (final XML row : new XMLDocument(table).nodes("/*/type[@id]")) {
                out.putAll(Lowered.provided(row));
            }
        }
        return out;
    }

    private static Map<String, String> provided(final XML row) {
        final Map<String, String> out = new HashMap<>(0);
        final String place = row.xpath("@id").get(0);
        for (final XML attr : row.nodes("attr[@void='true'][witnessed]")) {
            final Set<String> refs = new HashSet<>(attr.xpath("witnessed//ref/@loc"));
            final String kind = String.join("", refs);
            if (refs.size() == 1
                && ("Φ.number".equals(kind) || "Φ.bytes".equals(kind))) {
                out.put(
                    String.format("%s.%s", place, attr.xpath("@name").get(0)),
                    kind.substring(2)
                );
            }
        }
        return out;
    }

    private static List<Xnav> kids(final Xnav node) {
        return node.elements(Filter.withName("o")).collect(Collectors.toList());
    }
}
