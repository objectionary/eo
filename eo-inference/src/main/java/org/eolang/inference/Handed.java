/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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
 * <p>What the list says is a filling like any other and is spent among the
 * others, in the walk {@link Carried} makes and not after it. Both ends of the
 * walk need it there: which formation the atom is handed is itself carried in
 * by a hop at {@code malloc.eo:64}, and what the atom puts into that formation
 * is handed on again a line later, to the {@code m} of {@code [m] > x} that
 * Java fills with a chunk on every run (#8396).</p>
 *
 * @since 0.69.0
 */
final class Handed {

    /**
     * What stands between two members of a list.
     */
    private static final Pattern SPACE = Pattern.compile(" ");

    /**
     * The rows of the provides table that say what an atom hands in.
     */
    private final Collection<XML> annotated;

    /**
     * The rows of the provides table, asked what void a place lands in.
     */
    private final Provided provided;

    /**
     * Ctor.
     * @param links The links table, as {@link Resolved} left it
     * @param provides The provides table, which says what an atom hands in
     */
    Handed(final XML links, final XML provides) {
        this(
            provides.nodes("//attr[@void='true' and @args]"),
            new Provided(
                provides,
                new Ends(new Pairs(links).all()).names(),
                provides.xpath("//attr[@void='true']/@type")
            )
        );
    }

    /**
     * Ctor.
     * @param attrs The rows of the provides table that carry a brace list
     * @param rows The provides table, asked what void a place lands in
     */
    Handed(final Collection<XML> attrs, final Provided rows) {
        this.annotated = attrs;
        this.provided = rows;
    }

    /**
     * Put what the atoms hand in among the fillings the call sites name.
     * @param named What every void is filled with where a call site says so,
     *  which is what the atoms are added to
     * @param filled What every void is filled with, the hops walked through,
     *  which is where the formation an atom is handed is read from
     * @return Whether any void learnt anything it did not know before
     */
    boolean fills(
        final Map<String, Map<String, Type>> named,
        final Map<String, Map<String, Type>> filled
    ) {
        boolean grown = false;
        for (final Map.Entry<String, Collection<String>> hole : this.holes(filled).entrySet()) {
            final Map<String, Type> members = named.computeIfAbsent(
                hole.getKey(), key -> new LinkedHashMap<>(0)
            );
            for (final String handed : hole.getValue()) {
                if (members.putIfAbsent(handed, new Ref(handed)) == null) {
                    grown = true;
                }
            }
        }
        return grown;
    }

    private Map<String, Collection<String>> holes(final Map<String, Map<String, Type>> filled) {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final XML attr : this.annotated) {
            final Noted row = new Noted(attr);
            for (final Type filler
                : filled.getOrDefault(row.says("type"), Collections.emptyMap()).values()) {
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
