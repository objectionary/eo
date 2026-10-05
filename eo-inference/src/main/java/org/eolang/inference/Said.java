/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * What the links table says.
 *
 * <p>{@link Pairs} answers four questions off a document, which is the right
 * way round once and the wrong way round afterwards. The table is read from
 * disk once, and then a fixpoint asks the same four questions of a table that
 * exists only to be asked them: {@link Promoted} weaves the pairs it has
 * settled into rows, renders the rows into XML and reads the XML straight back
 * out, and on eo-runtime that round trip is most of a second of every pass of
 * a fixpoint that runs more than a hundred of them.</p>
 *
 * <p>So the four questions are asked of this instead, and a rule that has the
 * pairs can answer them without a document. Two of them are the pairs
 * themselves: what an object is a copy of is the pair, and what went into
 * every void is {@link Bound}'s answer read the other way round. The other two
 * do not depend on the pairs at all. Which rows are not pairs, and which of
 * those are answers as they stand, is what the table said when it was read
 * from disk, and no pass writes a row of either kind. That is why
 * {@link #with(Map, Map)} takes the first two afresh and keeps the last
 * two.</p>
 *
 * <p>A row that is not a pair wins over one that is, exactly as it does when
 * the rows are rendered, since a pass writes the rows it worked out first and
 * puts the ones it knows nothing about over the top. A row is not a pair when
 * the table gave it some form other than a reference, which is the same thing
 * {@link Pairs#others()} means by it. The order the pairs came in is kept,
 * because the first filling of a void is the one that counts.</p>
 *
 * @since 0.73.0
 */
final class Said {

    /**
     * What every object is a copy of.
     */
    private final Map<String, String> hops;

    /**
     * What went into every void.
     */
    private final Map<String, Collection<String>> given;

    /**
     * Which form of answer every row holds.
     */
    private final Map<String, String> shapes;

    /**
     * Every object the table answers by itself.
     */
    private final Collection<String> plain;

    /**
     * Ctor.
     *
     * @param table The links table, as a clue left it
     */
    Said(final Pairs table) {
        this(table.all(), table.puts(), table.forms(), table.certain());
    }

    /**
     * Ctor.
     *
     * @param copies What every object is a copy of
     * @param puts What went into every void
     * @param forms Which form of answer every row holds
     * @param certain Every object the table answers by itself
     */
    Said(
        final Map<String, String> copies,
        final Map<String, Collection<String>> puts,
        final Map<String, String> forms,
        final Collection<String> certain
    ) {
        this.hops = copies;
        this.given = puts;
        this.shapes = forms;
        this.plain = certain;
    }

    /**
     * The same table, saying what these pairs say.
     *
     * @param pairs The pairs, each object against the one it is a copy of
     * @param binds What every copy put into the voids of what it copies, from
     *  {@link Bound}
     * @return The table, with the pairs and their binds in place of its own and
     *  the rows no pass can write kept as they were
     */
    Said with(
        final Map<String, String> pairs,
        final Map<String, Map<String, String>> binds
    ) {
        final Map<String, String> copies = new LinkedHashMap<>(pairs.size());
        final Map<String, Collection<String>> puts = new LinkedHashMap<>(0);
        for (final Map.Entry<String, String> pair : pairs.entrySet()) {
            if (this.other(pair.getKey())) {
                continue;
            }
            copies.put(pair.getKey(), pair.getValue());
            binds.getOrDefault(pair.getKey(), Collections.emptyMap()).forEach(
                (hollow, put) -> puts.computeIfAbsent(
                    hollow, key -> new LinkedHashSet<>(0)
                ).add(put)
            );
        }
        return new Said(copies, puts, this.shapes, this.plain);
    }

    /**
     * Every pair of the table.
     *
     * @return The pairs, each name against the one it is a copy of
     */
    Map<String, String> all() {
        return this.hops;
    }

    /**
     * What the table says went into every void.
     *
     * @return The locators of what went in, by the locator of the void
     */
    Map<String, Collection<String>> puts() {
        return this.given;
    }

    /**
     * Which form of answer every row of the table holds.
     *
     * @return The form, by the locator of the object the row is about
     */
    Map<String, String> forms() {
        return this.shapes;
    }

    /**
     * Every object the table answers by itself.
     *
     * @return The locators
     */
    Collection<String> certain() {
        return this.plain;
    }

    private boolean other(final String object) {
        return this.shapes.containsKey(object) && !"ref".equals(this.shapes.get(object));
    }
}
