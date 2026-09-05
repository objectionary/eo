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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import org.w3c.dom.Node;
import org.xembly.Xembler;

/**
 * What an application through a void puts into the formation that fills it.
 *
 * <p>An application whose base is a void fills nothing on its own: the void
 * is not a formation and declares no places for an argument to land in. So
 * {@code cant-read "foo"}, written inside the {@code [^ cant-read] > as-ascii}
 * that takes it, was passed over and the {@code "foo"} went nowhere.</p>
 *
 * <p>A caller says what the void holds, though, and once it does the argument
 * has somewhere to go. An {@code as-ascii} given the formation
 * {@code "bar" > [message]} puts one void into {@code cant-read}, so the
 * {@code "foo"} above lands in that {@code message}, and nothing else in the
 * program ever says a word about it.</p>
 *
 * <p>The fact is written where {@link Bound} writes the same kind of fact, as
 * a {@code bind} in the row of the application, so everything downstream —
 * {@link Witnessed} above all — reads it without knowing it came from here.</p>
 *
 * <p>It is evidence and not a contract, as everything gathered from callers
 * is: the caller written tomorrow may put a formation of another shape
 * there.</p>
 *
 * <p>What a caller puts in is a locator of its own, so what fills the void is
 * looked up by the name it goes by. Only a formation written out at the call
 * site is its own name, and {@code malloc.for 0 x} is the common case: the
 * argument is a copy of an {@code x} written beside it, and the voids belong
 * to that (#8389).</p>
 *
 * @since 0.70.0
 */
public final class Relayed implements Clue {

    /**
     * The clues to follow first.
     */
    private final Clue origin;

    /**
     * Ctor.
     * @param clues The clues to follow before the arguments are passed on
     */
    public Relayed(final Clue clues) {
        this.origin = clues;
    }

    @Override
    public void follow(final Path xmirs, final Path tables) throws IOException {
        this.origin.follow(xmirs, tables);
        final Path table = tables.resolve("links.xml");
        final XML links = new XMLDocument(table);
        final XML given = new XMLDocument(tables.resolve("provides.xml"));
        final Pairs written = new Pairs(links);
        final Map<String, String> pairs = written.all();
        final List<String> voids = given.xpath("//attr[@void='true']/@type");
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(given, names, voids);
        final Collection<String> hollows = new HashSet<>(voids);
        final Map<String, Collection<String>> fillers = written.puts();
        final Map<String, Node> rows = written.refs();
        for (final Map.Entry<String, List<String>> application
            : new Given(new Xmirs(xmirs).applications()).arguments().entrySet()) {
            final String hollow = pairs.getOrDefault(application.getKey(), "");
            if (hollows.contains(hollow) && rows.containsKey(application.getKey())) {
                new Xembler(
                    new Passed(
                        owned,
                        names,
                        fillers.getOrDefault(hollow, Collections.emptyList()),
                        application.getValue()
                    ).directives()
                ).applyQuietly(rows.get(application.getKey()));
            }
        }
        Files.write(table, links.toString().getBytes(StandardCharsets.UTF_8));
    }
}
