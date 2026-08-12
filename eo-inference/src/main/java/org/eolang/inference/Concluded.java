/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

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
 * The clues, followed and then read together.
 *
 * <p>A clue writes down what it understands and decides nothing, which is
 * what keeps the rules simple and apart from one another. This is the other
 * half: the tables they wrote are read back, the checks in them are drained,
 * and what the checker is sure about lands in a table of its own beside them.
 * Nothing but the four documents passes between the two halves, so a rule can
 * change how it fills a table, or a table can gain a column, without the loop
 * being touched.</p>
 *
 * <p>The mistakes are written the way every other table is written, a type
 * with the attributes it is missing:</p>
 *
 * <pre> &lt;problems&gt;
 *   &lt;type id="Φ.app.t.next"&gt;
 *     &lt;attr name="foo" asked="Φ.app.inc.φ"/&gt;
 *   &lt;/type&gt;
 * &lt;/problems&gt;</pre>
 *
 * <p>which says that the object at {@code Φ.app.t.next} is asked for
 * {@code foo}, at {@code Φ.app.inc.φ}, and will never have it. The document
 * is written even when it is empty, because "nothing was found" is an answer
 * and a missing file is not. Nothing fails the build yet: the checker is a
 * prototype, and its verdicts are worth reading before they are worth
 * obeying.</p>
 *
 * <p>The loop leaves a second table behind it, of the same shape as the links
 * one, saying what the objects nobody wrote down turned out to be:</p>
 *
 * <pre> &lt;copies&gt;
 *   &lt;type id="Φ.use.φ.α0" copy="Φ.t.next"/&gt;
 * &lt;/copies&gt;</pre>
 *
 * <p>A rule cannot write that row: it takes a check to find out that the
 * {@code x.next} in one file is the {@code next} of a {@code t} in another.
 * It is kept apart from the links the rules wrote for the same reason the
 * rules are kept apart from each other — one document, one author.</p>
 *
 * @since 0.68.0
 */
public final class Concluded implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     * @param clues The clues to follow before the checks are drained
     */
    public Concluded(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        try (
            Tojos rows = new TjDeferred(new MnMemory());
            Tojos found = new TjDeferred(new MnMemory())
        ) {
            for (final Map.Entry<String, String> copy
                : new Settled(
                    new XMLDocument(tables.resolve("provides.xml")),
                    new XMLDocument(tables.resolve("needs.xml")),
                    new XMLDocument(tables.resolve("checks.xml")),
                    new Same(new XMLDocument(tables.resolve("links.xml"))).names()
                ).drain(rows).entrySet()) {
                found.add(copy.getKey()).set("copy", copy.getValue());
            }
            Files.write(
                tables.resolve("problems.xml"),
                new Grouped(rows, "problems").asXml().toString().getBytes(StandardCharsets.UTF_8)
            );
            Files.write(
                tables.resolve("copies.xml"),
                new Grouped(found, "copies").asXml().toString().getBytes(StandardCharsets.UTF_8)
            );
        }
    }
}
