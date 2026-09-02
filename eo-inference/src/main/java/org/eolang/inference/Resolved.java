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
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * The links, closed under what a dispatch turns out to be.
 *
 * <p>A rule writes down what it reads in the text and nothing else, which
 * leaves a gap no rule can fill: an object the program computes. A dispatch
 * {@code .if} is made while the program runs, and the tables know only that
 * somebody asked {@code if} of something, never what came back. So a chain of
 * them stops at the first one, and an object that hands its answers to a
 * dispatch hands them to a locator nothing describes.</p>
 *
 * <p>The fact is there to be had, and no call site is needed for it, which is
 * what {@link Dispatched} works out. Answering {@code a.b.c} needs {@code a.b}
 * answered first, and one pass cannot put them in that order, so passes run
 * until one of them adds nothing. Pairs are only ever added, of which there
 * are finitely many, so it settles.</p>
 *
 * <p>What comes out belongs in the document the rules already keep for pairs,
 * rather than beside it: two documents saying "is a copy of" in different
 * words would only invite the two to disagree. So this reads the tables and
 * writes {@code links.xml} back, with the pairs it worked out added to the
 * ones the rules found.</p>
 *
 * <p>What every application fills is written here as well, and not by a rule
 * of its own, for want of anywhere earlier to write it: naming the void an
 * argument lands in means knowing which formation is being copied, and that is
 * what the pairs have just settled.</p>
 *
 * <p>So is the admission that a dispatch could not be worked out, for the same
 * reason in reverse: only here, when the passes have stopped adding pairs, is
 * it known that no pass will answer it. A row saying nothing is known is worth
 * writing, since an absent row says that and also says "nobody looked", and a
 * reader has no way of telling which.</p>
 *
 * @since 0.68.0
 */
public final class Resolved implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     * @param clues The clues to follow before the links are closed
     */
    public Resolved(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Path links = tables.resolve("links.xml");
        final Xmirs world = new Xmirs(xmirs);
        final XML given = new XMLDocument(tables.resolve("provides.xml"));
        final Collection<Site> dispatches = world.dispatches();
        final Given applied = new Given(world.applications());
        final Map<String, List<String>> args = applied.arguments();
        final Map<String, Map<String, String>> named = applied.named();
        final Map<String, String> receivers = world.receivers();
        final List<String> voids = given.xpath("//attr[@void='true']/@type");
        final Pairs written = new Pairs(new XMLDocument(links));
        final Map<String, String> pairs = new Settled(
            new Dispatched(given, dispatches, args, named, receivers, voids)
        ).from(
            new Settled(
                new Dispatched(
                    given, dispatches, args, named, receivers, Collections.emptyList()
                )
            ).from(written.all())
        );
        final Map<String, String> names = new Ends(pairs).names();
        final Map<String, Type> rows = new Refs(
            pairs,
            new Bound(
                args, named, receivers, pairs, new Provided(given, names, voids)
            ).all()
        ).all();
        rows.putAll(written.others());
        final Collection<String> dead = new Dead(written, dispatches, names).all();
        for (final Site dispatch : dispatches) {
            final String made = dispatch.made();
            if (dead.contains(made)) {
                rows.put(made, new Terminator());
            } else {
                rows.putIfAbsent(made, new Unknown());
            }
        }
        Files.write(
            links,
            new Types(rows).asXml().toString().getBytes(StandardCharsets.UTF_8)
        );
    }
}
