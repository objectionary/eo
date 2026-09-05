/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Pattern;

/**
 * What an atom puts into the voids of what it is handed.
 *
 * <p>{@link Fillings} reads the applications of a program, and an atom is not
 * one of them. Its body is Java, so a formation given to it is called where no
 * source can be read, and the voids of that formation are filled by nobody as
 * far as the tables can see. The source says so instead:</p>
 *
 * <pre> [] &gt; of /Q.bytes
 *   ? &gt; size /Q.number
 *   ? &gt; scope /{Q.chunk}</pre>
 *
 * <p>The braces are the list of what the atom hands in, one member to a place,
 * which is exactly what {@code EOmalloc$EOof} does with a
 * {@code scope.put(0, chunk)}. So whatever fills {@code scope} has its own
 * first void filled with a {@code Φ.chunk}, and {@link Provided} names that
 * void the way a positional argument finds one.</p>
 *
 * <p>A member that names no object is passed over. A letter stands for a type
 * the atom carries from one void to another and says nothing about what is put
 * in, which is a question for whoever fills the void the letter came from.</p>
 *
 * @since 0.69.0
 */
final class Handed {

    /**
     * What stands between two members of a list.
     */
    private static final Pattern SPACE = Pattern.compile(" ");

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * The rows of the provides table, asked what void a place lands in.
     */
    private final Provided provided;

    /**
     * What the applications of the program put into every void.
     */
    private final Map<String, Collection<Type>> filled;

    /**
     * Ctor.
     * @param links The links table, as {@link Resolved} left it
     * @param provides The provides table, which says what an atom hands in
     * @param found What is put into every void, from {@link Fillings}
     */
    Handed(final XML links, final XML provides, final Map<String, Collection<Type>> found) {
        this(
            provides,
            new Provided(
                provides,
                new Ends(new Pairs(links).all()).names(),
                provides.xpath("//attr[@void='true']/@type")
            ),
            found
        );
    }

    /**
     * Ctor.
     * @param provides The provides table, which says what an atom hands in
     * @param rows The provides table, asked what void a place lands in
     * @param found What is put into every void, from {@link Fillings}
     */
    Handed(final XML provides, final Provided rows, final Map<String, Collection<Type>> found) {
        this.given = provides;
        this.provided = rows;
        this.filled = found;
    }

    /**
     * What is ever put into every void, by the program and by the atoms alike.
     * @return The types put in, by the locator of the void
     */
    Map<String, Collection<Type>> all() {
        final Map<String, Collection<Type>> found = new LinkedHashMap<>(this.filled);
        for (final Map.Entry<String, Collection<String>> hole : this.holes().entrySet()) {
            final Collection<Type> members = new ArrayList<>(
                found.getOrDefault(hole.getKey(), Collections.emptyList())
            );
            final Collection<String> seen = new HashSet<>(0);
            for (final Type member : members) {
                seen.add(member.names());
            }
            for (final String handed : hole.getValue()) {
                if (seen.add(handed)) {
                    members.add(new Ref(handed));
                }
            }
            found.put(hole.getKey(), members);
        }
        return found;
    }

    private Map<String, Collection<String>> holes() {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final XML attr : this.given.nodes("//attr[@void='true' and @args]")) {
            final Noted row = new Noted(attr);
            for (final Type filler
                : this.filled.getOrDefault(row.says("type"), Collections.emptyList())) {
                this.landed(filler.names(), row.says("args")).forEach(
                    (hollow, member) -> found
                        .computeIfAbsent(hollow, key -> new ArrayList<>(0))
                        .add(member)
                );
            }
        }
        return found;
    }

    private Map<String, String> landed(final String filler, final String args) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        final String[] members = Handed.SPACE.split(args, -1);
        for (int place = 0; place < members.length; place = place + 1) {
            final String hollow = this.provided.vacant(
                filler, Collections.emptyList(), place
            );
            if (!hollow.isEmpty() && members[place].startsWith("Φ.")) {
                found.put(hollow, members[place]);
            }
        }
        return found;
    }
}
