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
        final XML links = new XMLDocument(tables.resolve("links.xml"));
        final Map<String, String> names = new Ends(new Pairs(links).all()).names();
        final Collection<String> voids = given.xpath("//attr[@void='true']/@type");
        final Map<String, String> into = this.into(links, names, voids);
        final Map<String, Map<String, String>> asked = new Asked(
            new XMLDocument(tables.resolve("needs.xml")),
            names,
            new Provided(given, names, voids)
        ).all();
        for (final XML hollow : given.nodes("//attr[@void='true']")) {
            final Demands demands = new Demands(
                asked, this.roots(hollow.xpath("@type").get(0), into)
            );
            if (demands.any()) {
                new Xembler(demands.directives()).applyQuietly(hollow.inner());
            }
        }
        Files.write(table, given.toString().getBytes(StandardCharsets.UTF_8));
    }

    private Map<String, String> into(
        final XML links, final Map<String, String> names, final Collection<String> voids
    ) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        for (final XML bind : links.nodes("/links/type/ref/bind[ref]")) {
            final String loc = bind.xpath("ref/@loc").get(0);
            final String filler = names.getOrDefault(loc, loc);
            if (voids.contains(filler)) {
                found.put(filler, bind.xpath("@void").get(0));
            }
        }
        return found;
    }

    private Collection<String> roots(final String hollow, final Map<String, String> into) {
        final Collection<String> found = new LinkedHashSet<>(0);
        String walked = hollow;
        while (found.add(walked)) {
            if (!into.containsKey(walked)) {
                break;
            }
            walked = into.get(walked);
        }
        return found;
    }
}
