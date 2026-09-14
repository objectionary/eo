/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

/**
 * Every fragment of one document lowered by runs of phino.
 *
 * <p>It takes the phino binary, the formas of the build, the directory to
 * work in and the name of the document. Given the document, it rewrites it
 * in place — each fragment phino could reduce becomes a call of a
 * generated atom — and answers how many fragments changed. A fragment
 * phino refuses, or one Java cannot spell, is left as written and the next
 * one is tried.</p>
 *
 * @since 0.77.0
 * @todo #8548:30min Lower a const handle that is the whole answer of the
 *  body, as in the forced-bool and forced-number packs: phino reduces
 *  {@code dataized(x).as-bytes} to a formation whose φ is its hidden ρ,
 *  the bytes, and the engine finds no data to splice, so
 *  the fragment stays as written. Find out whether phino should reduce
 *  through the φ of that formation or the engine should ask for its ρ,
 *  then set the expectations of those two packs to the lowered form.
 */
public final class Lowered implements Rewrite {

    /**
     * The binary.
     */
    private final Phino phino;

    /**
     * The formas of the build.
     */
    private final Formas formas;

    /**
     * The directory of the build.
     */
    private final Home home;

    /**
     * The identifier of the document.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param exe The binary
     * @param tables The formas of the build
     * @param dir The directory of the build
     * @param identifier The identifier of the document
     */
    public Lowered(final Phino exe, final Formas tables, final Home dir,
        final String identifier) {
        this.phino = exe;
        this.formas = tables;
        this.home = dir;
        this.name = identifier;
    }

    @Override
    public int rewrite(final Xnav doc) throws IOException {
        final Document document = Lowered.owner(doc.node());
        final Boxes boxes = new Boxes(this.home.boxes());
        new Trips(this.home.trips(this.name)).reset();
        int done = 0;
        for (final Box box : boxes.all()) {
            if (!Lowered.owns(document, box.locator())) {
                continue;
            }
            final Element original = new Located(
                document.getDocumentElement(), box.locator()
            ).element();
            final Element copy = (Element) original.cloneNode(true);
            original.getParentNode().replaceChild(copy, original);
            boolean made = false;
            try {
                made = this.lowered(document, copy, boxes, box.locator()) > 0;
            } catch (final IOException | IllegalStateException ex) {
                Logger.debug(
                    this, "The fragment at %s stays as written: %[exception]s",
                    box.locator(), ex
                );
            }
            if (made) {
                ++done;
            } else {
                copy.getParentNode().replaceChild(original, copy);
            }
        }
        return done;
    }

    private int lowered(final Document document, final Element fragment,
        final Boxes boxes, final String locator) throws IOException {
        final Path run = this.home.run();
        try {
            final Symbols symbols = new Symbols(run.resolve("symbols.tsv"));
            final Path variant = run.resolve("fragment.xmir");
            new Xml(new Boxed(document, boxes, locator).copy()).saved(variant);
            final List<Path> docs = new ArrayList<>(this.home.others(this.name));
            docs.add(variant);
            final Path world = run.resolve("world.phi");
            this.phino.merged(docs, world);
            final Path registry = new Registry(
                run, run.resolve("symbols.tsv"), this.home.boxes(),
                this.home.trips(this.name)
            ).saved();
            final String entry = new Applied(document, locator, this.formas, symbols).phi();
            final List<String> residuals = new ArrayList<>(0);
            for (final Element kid : new Kids(fragment)) {
                if (!Lowered.morphable(kid)) {
                    continue;
                }
                final String binding = kid.getAttribute("name");
                final String residual = this.phino.morphed(
                    world, String.format("%s.%s", entry, binding), registry
                );
                Logger.debug(this, "The residual of %s.%s is: %s", locator, binding, residual);
                if (!residual.contains("⊥")) {
                    residuals.add(String.format("%s ↦ %s", binding, residual));
                }
            }
            int out = 0;
            if (!residuals.isEmpty()) {
                final Path phi = run.resolve("residual.phi");
                Files.write(
                    phi,
                    String.format(
                        "⟦ residual ↦ ⟦ %s ⟧, ρ ↦ ∅ ⟧", String.join(", ", residuals)
                    ).getBytes(StandardCharsets.UTF_8)
                );
                new Splice(
                    fragment,
                    (Element) document.importNode(
                        new Xnav(this.phino.xmir(phi)).element("object").element("o").node(),
                        true
                    )
                ).apply();
                out = new Marked(fragment, new Table(symbols), this.home.atoms()).apply();
            }
            return out;
        } finally {
            Lowered.deleted(run);
        }
    }

    private static boolean morphable(final Element binding) {
        boolean out = binding.hasAttribute("name")
            && !"λ".equals(binding.getAttribute("name"))
            && !"ρ".equals(binding.getAttribute("name"))
            && !"∅".equals(binding.getAttribute("base"));
        if (out && !binding.hasAttribute("base")) {
            for (final Element kid : new Kids(binding)) {
                if ("∅".equals(kid.getAttribute("base")) || "λ".equals(kid.getAttribute("name"))) {
                    out = false;
                    break;
                }
            }
        }
        return out;
    }

    private static boolean owns(final Document document, final String locator) {
        boolean out = false;
        for (final Element top : new Kids(document.getDocumentElement())) {
            final String place = top.getAttribute("loc");
            if (locator.equals(place) || locator.startsWith(String.format("%s.", place))) {
                out = true;
                break;
            }
        }
        return out;
    }

    private static Document owner(final Node node) {
        final Document out;
        if (node.getNodeType() == Node.DOCUMENT_NODE) {
            out = (Document) node;
        } else {
            out = node.getOwnerDocument();
        }
        return out;
    }

    private static void deleted(final Path dir) throws IOException {
        try (Stream<Path> files = Files.walk(dir)) {
            for (final Path file : files.sorted(Comparator.reverseOrder()).toArray(Path[]::new)) {
                Files.delete(file);
            }
        }
    }
}
