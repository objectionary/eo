/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import java.io.IOException;
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
 * Every fragment of one XMIR document lowered by a run of phino: the
 * document is copied with the fragment left open and every other formation
 * boxed, its voids are planted with markers, the copy is merged with the
 * boxed variants of every other document into a world, phino morphs the
 * fragment inside that world through the engine, and the residual comes
 * back into the document with each marker turned into an atom.
 *
 * <p>A fragment phino cannot reduce, or whose program Java cannot render,
 * stays as written, and the next fragment is tried, since each run is
 * independent of the others: the boxes it enters are served by the engine
 * from the tables, not from the results of earlier runs.</p>
 *
 * @since 0.77.0
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
     * @checkstyle ParameterNumberCheck (5 lines)
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
        int done = 0;
        for (final Box box : boxes.all()) {
            if (!this.owns(document, box.locator())) {
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
            final Document variant = new Boxed(document, boxes, locator).copy();
            new Symbolized(variant, locator, this.formas, symbols).plant();
            final Path planted = run.resolve("fragment.xmir");
            new Xml(variant).saved(planted);
            final List<Path> docs = new ArrayList<>(this.home.others(this.name));
            docs.add(planted);
            final Path world = run.resolve("world.phi");
            this.phino.merged(docs, world);
            final String residual = this.phino.morphed(
                world, locator,
                new Registry(run, run.resolve("symbols.tsv"), this.home.boxes()).saved()
            );
            Logger.debug(this, "The residual of %s is: %s", locator, residual);
            if (residual.contains("⊥")) {
                throw new IllegalStateException(
                    String.format("The residual of %s reaches the terminator", locator)
                );
            }
            new Splice(
                fragment, (Element) new Xnav(residual).element("object").element("o").node()
            ).apply();
            return new Marked(fragment, new Table(symbols), this.home.atoms()).apply();
        } finally {
            Lowered.deleted(run);
        }
    }

    private boolean owns(final Document document, final String locator) {
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
