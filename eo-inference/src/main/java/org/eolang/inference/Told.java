/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * The rows about a void, with the object its callers settle it at.
 *
 * <p>A void row says what it holds only when the source wrote it down, and
 * everything the passes learn about the same void arrives beside it as a census
 * {@link Witnessed} writes. Of the 2,038 void rows of eo-runtime 636 carry the
 * annotation and 711 more are settled by their census alone, so the answer is
 * in the table and nobody has said it. {@link Ones} works it out and this
 * writes it down, as one more cell of the row:</p>
 *
 * <pre> &lt;attr name="x" type="Φ.inc.x" void="true" settled="Φ.number"/&gt;</pre>
 *
 * <p>A cell of its own and not the {@code holds} the source writes, because a
 * declaration is true of every caller there will ever be and a sighting only of
 * the callers this program happens to have. {@link Answers} lets the annotation
 * win where they disagree, and {@link Held} and {@link Provided} walk through a
 * void on what it declares, so a row the source typed is left alone.</p>
 *
 * @since 0.74.0
 */
public final class Told implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     *
     * @param clues The clues to follow before the rows are told
     */
    public Told(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Path table = tables.resolve("provides.xml");
        final XML given = new XMLDocument(table);
        final Map<String, String> ones = new Ones(given).all();
        for (final Xnav type : new Rows(given).all()) {
            type.elements(Filter.withName("attr"))
                .filter(attr -> "true".equals(new Noted(attr).says("void")))
                .filter(attr -> new Noted(attr).says("holds").isEmpty())
                .filter(attr -> ones.containsKey(new Noted(attr).says("type")))
                .forEach(hollow -> Told.settle(hollow, ones.get(new Noted(hollow).says("type"))));
        }
        Files.write(table, given.toString().getBytes(StandardCharsets.UTF_8));
    }

    private static void settle(final Xnav hollow, final String sole) {
        new Xembler(new Directives().attr("settled", sole)).applyQuietly(hollow.node());
    }
}
