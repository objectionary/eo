/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjDeferred;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

/**
 * The tables, read together for mistakes.
 *
 * <p>A clue writes down what it understands and decides nothing, which is what
 * keeps the rules simple and apart from one another. This is the other half:
 * the tables they wrote are read back and what is certainly wrong lands in a
 * table of its own beside them. Nothing but the documents passes between the
 * two halves, so a rule can change how it fills a table, or a table can gain a
 * column, without this being touched.</p>
 *
 * <p>The mistakes are written the way every other table is written, a type
 * with the names it is asked for and will never have:</p>
 *
 * <pre> &lt;problems&gt;
 *   &lt;type id="Φ.t"&gt;
 *     &lt;attr name="extra" asked="Φ.app.φ"/&gt;
 *   &lt;/type&gt;
 * &lt;/problems&gt;</pre>
 *
 * <p>which says that {@code extra} is taken from a {@code Φ.t}, at
 * {@code Φ.app.φ}, and that a {@code Φ.t} will never have it. The document is
 * written even when it is empty, because "nothing was found" is an answer and
 * a missing file is not. Nothing fails the build: this is a prototype, and its
 * verdicts are worth reading before they are worth obeying.</p>
 *
 * <p>This was written once before and taken out again in #6661, when it
 * reported nothing at all: a verdict needs the object that misses an attribute
 * to have been seen whole, and hardly any object had been. It needed a
 * worklist then — a queue of promises filed by every application, each split
 * into smaller ones — to push a type into the body it was passed to. The
 * tables now answer without being pushed, so what is left of it is
 * {@link Missing}, one pass over the rows.</p>
 *
 * @since 0.69.0
 */
public final class Concluded implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     * @param clues The clues to follow before the tables are read
     */
    public Concluded(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final XML given = new XMLDocument(tables.resolve("provides.xml"));
        final Map<String, String> names = new Ends(
            new Pairs(new XMLDocument(tables.resolve("links.xml"))).all()
        ).names();
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            new Missing(
                new Provided(given, names),
                new Ungrouped(new XMLDocument(tables.resolve("needs.xml")), names).rows()
            ).fill(rows);
            Files.write(
                tables.resolve("problems.xml"),
                new Grouped(rows, "problems").asXml().toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }
}
