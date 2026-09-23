/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Filter;
import com.github.lombrozo.xnav.Xnav;
import com.jcabi.log.Logger;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * The names of the voids that positional arguments land in.
 *
 * <p>Every application in {@code links.xml} is a row saying what it is a copy
 * of and, in {@code bind} elements, which void of that copy each of its
 * arguments fills:</p>
 *
 * <pre> &lt;type id="Φ.app.φ"&gt;
 *   &lt;ref loc="Φ.app.foo"&gt;
 *     &lt;bind void="Φ.app.foo.bar"&gt;
 *       &lt;ref loc="Φ.app.φ.α0"/&gt;
 *     &lt;/bind&gt;
 *   &lt;/ref&gt;
 * &lt;/type&gt;</pre>
 *
 * <p>Read backwards, the row says that {@code Φ.app.φ.α0} is {@code bar}.
 * Only a void of the formation the application ends up copying counts, after
 * the whole chain of copies is followed: {@code half 2}, where {@code half}
 * is {@code pair 1}, fills a void of {@code pair}. A void of any other
 * formation is evidence gathered from callers, where an argument of a void
 * is passed on to every formation somebody puts in that void, and the next
 * caller may put one with a void of another name there. An argument landing
 * in two voids at once has no name either.</p>
 *
 * <p>A {@code ref} or a {@code bind} marked {@code witnessed="true"} was
 * reached only through what the program was seen to put into a void, so it is
 * left out, together with the copies that pass through it.</p>
 *
 * @since 0.69.0
 */
final class Landings {

    /**
     * The path of {@code links.xml}.
     */
    private final Path links;

    /**
     * Ctor.
     *
     * @param path The path of {@code links.xml}
     */
    Landings(final Path path) {
        this.links = path;
    }

    /**
     * The name of the void of every argument that lands in exactly one.
     *
     * @return The names, by the locator of the argument
     * @throws IOException If the table can't be read
     */
    Map<String, String> names() throws IOException {
        if (!Files.exists(this.links)) {
            throw new IOException(
                Logger.format(
                    "The table %[file]s is absent, while the arguments can be named only after the 'inference' goal has written it",
                    this.links
                )
            );
        }
        final Xnav table = new Xnav(new XMLDocument(this.links).inner()).element("links");
        final Map<String, String> copies = new HashMap<>(0);
        table.elements(Filter.withName("type"))
            .filter(row -> Landings.certain(row.element("ref"))).forEach(
                row -> row.element("ref").attribute("loc").text().ifPresent(
                    loc -> copies.put(row.attribute("id").text().get(), loc)
                )
            );
        final Map<String, Collection<String>> found = new HashMap<>(0);
        table.elements(Filter.withName("type"))
            .filter(row -> Landings.certain(row.element("ref"))).forEach(
                row -> {
                    final String end = Landings.end(copies, row.attribute("id").text().get());
                    row.element("ref").elements(Filter.withName("bind"))
                        .filter(Landings::certain)
                        .forEach(bind -> Landings.landed(found, end, bind));
                }
            );
        final Map<String, String> names = new HashMap<>(found.size());
        for (final Map.Entry<String, Collection<String>> arg : found.entrySet()) {
            if (arg.getValue().size() == 1) {
                names.put(arg.getKey(), arg.getValue().iterator().next());
            }
        }
        return names;
    }

    private static void landed(
        final Map<String, Collection<String>> found, final String end, final Xnav bind
    ) {
        final String hollow = bind.attribute("void").text().orElse("");
        final int dot = hollow.lastIndexOf('.');
        if (dot > 0 && hollow.substring(0, dot).equals(end)) {
            bind.element("ref").attribute("loc").text().ifPresent(
                arg -> found.computeIfAbsent(arg, key -> new LinkedHashSet<>(1))
                    .add(hollow.substring(dot + 1))
            );
        }
    }

    private static boolean certain(final Xnav link) {
        return !"true".equals(link.attribute("witnessed").text().orElse(""));
    }

    private static String end(final Map<String, String> copies, final String name) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = name;
        while (copies.containsKey(walked) && seen.add(walked)) {
            walked = copies.get(walked);
        }
        return walked;
    }
}
