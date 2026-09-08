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
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import org.xembly.Xembler;

/**
 * The rows about a void, with what the program asks of it.
 *
 * <p>{@link Provides} writes a void down as an attribute nobody has filled and
 * has nothing more to say, since what a void holds is decided elsewhere. What
 * it will have to <em>offer</em> is not decided elsewhere at all — it is
 * written all over the program, one name at a time, and {@link Needs} has every
 * one of those names against the site that asked. Turning that round and
 * putting it where it belongs is all this does.</p>
 *
 * <p>It waits until the links are settled, since a site asks a receiver and
 * only the links say where the asking arrived: a {@code book} that binds no
 * {@code size} hands the question to the void it keeps, and it is that void
 * which will have to answer. And it writes into the table the voids are
 * already in, because a demand is a fact about a void, and a second table
 * saying things about voids would only invite the two to disagree.</p>
 *
 * <p>A name is one of two things the program asks of a void, and the other is
 * a call: {@code ^.body index} applies whatever fills {@code body} rather than
 * asking it for a name. {@link Needs} keeps no row for that, since it gathers
 * dispatches, so the calls are read off the applications of the program here
 * and written on the same rows by {@link Applies} (#8158).</p>
 *
 * @since 0.69.0
 */
public final class Demanded implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     * @param clues The clues to follow before the voids are asked of
     */
    public Demanded(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Path table = tables.resolve("provides.xml");
        final XML given = new XMLDocument(table);
        final Pairs links = new Pairs(new XMLDocument(tables.resolve("links.xml")));
        final Map<String, String> names = new Ends(links.all()).names();
        final Collection<String> voids = given.xpath("//attr[@void='true']/@type");
        final Map<String, Collection<String>> into = Demanded.into(links.puts(), names, voids);
        final Map<String, Map<String, String>> asked = new Asked(
            new XMLDocument(tables.resolve("needs.xml")),
            names,
            new Provided(given, names, voids)
        ).all();
        final Collection<Call> calls = new Calls(
            new Xmirs(xmirs).applications(), links, given
        ).all();
        for (final XML hollow : given.nodes("//attr[@void='true']")) {
            final Rooted rooted = new Rooted(
                Demanded.roots(new Noted(hollow).says("type"), into)
            );
            final Demands demands = new Demands(asked, rooted);
            if (demands.any()) {
                new Xembler(demands.directives()).applyQuietly(hollow.inner());
            }
            final Applies applies = new Applies(calls, rooted);
            if (applies.any()) {
                new Xembler(applies.directives()).applyQuietly(hollow.inner());
            }
        }
        Files.write(table, given.toString().getBytes(StandardCharsets.UTF_8));
    }

    private static Map<String, Collection<String>> into(
        final Map<String, Collection<String>> puts,
        final Map<String, String> names,
        final Collection<String> voids
    ) {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Collection<String>> bound : puts.entrySet()) {
            for (final String put : bound.getValue()) {
                final String filler = names.getOrDefault(put, put);
                if (voids.contains(filler)) {
                    found.computeIfAbsent(filler, key -> new LinkedHashSet<>(0))
                        .add(bound.getKey());
                }
            }
        }
        return found;
    }

    private static Collection<String> roots(
        final String hollow, final Map<String, Collection<String>> into
    ) {
        final Collection<String> found = new LinkedHashSet<>(0);
        final Deque<String> left = new ArrayDeque<>(Collections.singletonList(hollow));
        while (!left.isEmpty()) {
            final String walked = left.removeFirst();
            if (found.add(walked)) {
                left.addAll(into.getOrDefault(walked, Collections.emptyList()));
            }
        }
        return found;
    }
}
