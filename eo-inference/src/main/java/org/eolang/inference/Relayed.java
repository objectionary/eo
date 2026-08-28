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
import org.xembly.Xembler;

/**
 * What an application through a void puts into the formation that fills it.
 *
 * <p>An application whose base is a void fills nothing on its own: the void
 * is not a formation and declares no places for an argument to land in. So
 * {@code cant-read "foo"}, written inside a formation that takes
 * {@code cant-read}, was passed over and the {@code "foo"} went nowhere:</p>
 *
 * <pre> [^ cant-read] &gt; as-ascii
 *   cant-read "foo" &gt; @</pre>
 *
 * <p>A caller says what the void holds, though, and once it does the argument
 * has somewhere to go. An {@code as-ascii} whose argument is the formation
 * {@code "bar" > [message]} puts a formation of one void into
 * {@code cant-read}, so the {@code "foo"} of the application
 * above lands in the {@code message} of that formation, and the tables say
 * nothing about {@code message} until they say that.</p>
 *
 * <p>The fact is written where {@link Bound} writes the same kind of fact, as
 * a {@code bind} in the row of the application, so everything downstream reads
 * it without knowing it came from here — {@link Witnessed} above all, which is
 * where the evidence about a void ends up.</p>
 *
 * <p>It is evidence and not a contract, as everything gathered from callers
 * is. A void filled with a formation of one void by every caller a program has
 * today is still a void, and the caller written tomorrow may put a formation
 * of three there.</p>
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
        final Map<String, String> pairs = new Pairs(links).all();
        final List<String> voids = given.xpath("//attr[@void='true']/@type");
        final Provided owned = new Provided(given, new Ends(pairs).names(), voids);
        final Collection<String> hollows = new HashSet<>(voids);
        final Map<String, Collection<String>> fillers = new Fillers(links).all();
        for (final Map.Entry<String, List<String>> application
            : new Given(new Xmirs(xmirs).applications()).arguments().entrySet()) {
            final String hollow = pairs.getOrDefault(application.getKey(), "");
            final Collection<XML> rows = links.nodes(
                String.format("/links/type[@id='%s']/ref", application.getKey())
            );
            if (hollows.contains(hollow) && !rows.isEmpty()) {
                new Xembler(
                    new Passed(
                        owned,
                        fillers.getOrDefault(hollow, Collections.emptyList()),
                        application.getValue()
                    ).directives()
                ).applyQuietly(rows.iterator().next().inner());
            }
        }
        Files.write(table, links.toString().getBytes(StandardCharsets.UTF_8));
    }
}
