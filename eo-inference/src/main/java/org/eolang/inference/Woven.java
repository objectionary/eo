/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.Map;

/**
 * The rows that follow from a set of pairs.
 *
 * <p>What an object is a copy of and which voids that copy has filled are two
 * halves of one row, and putting the halves together takes the provides table,
 * every application of the program and the pairs themselves. {@link Refs} joins
 * them, {@link Bound} works out what went where and {@link Provided} says which
 * voids there were to fill; this is the four of them wired up, so that whoever
 * has pairs and wants rows says so in one line.</p>
 *
 * <p>Rows are asked for twice over. Once at the end, for the table the build
 * writes down, and once for every provisional table a fact is read off before
 * that: what the program puts into a void is written in the rows of the objects
 * that put it there, so learning it means having the rows already. Both come
 * from here, because a fact read off a table the build does not go on to
 * publish would be a fact nobody can check.</p>
 *
 * @since 0.71.0
 */
final class Woven {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * What every application of the program gives, from {@link Given}.
     */
    private final Given applied;

    /**
     * What every dispatch takes its attribute from, from {@link Xmirs}.
     */
    private final Map<String, String> receivers;

    /**
     * The locator of every void, from {@link Hollows}.
     */
    private final Collection<String> hollows;

    /**
     * Ctor.
     * @param provides The provides table, as {@link Provides} wrote it
     * @param applications What every application of the program gives
     * @param taken What every dispatch takes its attribute from
     * @param voids The locator of every void
     */
    Woven(
        final XML provides,
        final Given applications,
        final Map<String, String> taken,
        final Collection<String> voids
    ) {
        this.given = provides;
        this.applied = applications;
        this.receivers = taken;
        this.hollows = voids;
    }

    /**
     * These pairs as the rows of the links table.
     * @param pairs The pairs, each object against the one it is a copy of
     * @return The types, by the locator of the object they are about, in the
     *  order the pairs came in
     */
    Map<String, Type> rows(final Map<String, String> pairs) {
        return new Refs(
            pairs,
            new Bound(
                this.applied.arguments(),
                this.applied.named(),
                this.receivers,
                pairs,
                new Provided(this.given, new Ends(pairs).names(), this.hollows)
            ).all()
        ).all();
    }
}
