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
 * The links, with a row that copies a void saying what the void holds.
 *
 * <p>A row calling an object a copy of a void says everything the text says
 * and less than the program does. The body of {@code [item] > box} hands back
 * the {@code item} it was given, so the row of that body says {@code Φ.box.item}
 * and a reader arrives at a void; but a build reads the program whole, library
 * and all, and where every caller of {@code box} puts a {@code bell} there the
 * body is a {@code bell}. {@link Ones} is which voids those are, and 2,368 rows
 * of eo-runtime name one of them and stop.</p>
 *
 * <p>The hop is spent at the end and not while the passes run, because two
 * readers want the row to say two different things. {@link Demanded} works out
 * what a void owes from the rows that read names off it, and a row that has
 * stopped naming the void owes it nothing, so filling the voids any earlier
 * would be taking their demands away from them. The demands are written first,
 * off the rows the rules wrote, and the rows are told what the census knows
 * afterwards.</p>
 *
 * <p>The void keeps its own row, which still says it is a void. What goes into
 * one is gathered from its callers and is a fact about them, and the next
 * caller is free to bring something else. A row carrying a choice of arms is
 * left alone as well, a choice being what a row says when the census has more
 * than one member to offer (#8744).</p>
 *
 * @since 0.74.0
 */
public final class Named implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     *
     * @param clues The clues to follow before the rows are told
     */
    public Named(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Map<String, String> ones = new Ones(
            new XMLDocument(tables.resolve("provides.xml"))
        ).all();
        final Path links = tables.resolve("links.xml");
        final XML table = new XMLDocument(links);
        for (final Xnav row : new Rows(table).all()) {
            row.elements(Filter.withName("ref")).forEach(ref -> Named.told(ref, ones));
        }
        Files.write(links, table.toString().getBytes(StandardCharsets.UTF_8));
    }

    private static void told(final Xnav ref, final Map<String, String> ones) {
        final String sole = ones.getOrDefault(new Noted(ref).says("loc"), "");
        if (!sole.isEmpty() && !ref.elements(Filter.withName("union")).findAny().isPresent()) {
            new Xembler(new Directives().attr("loc", sole)).applyQuietly(ref.node());
        }
    }
}
