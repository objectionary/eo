/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjDeferred;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

/**
 * The checks, drained until the loop stops learning.
 *
 * <p>{@link Worklist} works out what a computed object is a copy of every time
 * it resolves an attribute, and one draining is not enough to use that: the
 * check that needed the fact may have been decided before the check that found
 * it. So the list is drained again, with what the last draining learned added
 * to the names, and again, until a draining brings nothing new. Each one starts
 * from the tables as they were written, so a fact never rests on a fact that a
 * later draining takes back.</p>
 *
 * <p>A locator that came out two ways is not learned at all. The dispatch
 * {@code x.next} is one object in the program text, and a formation copied
 * twice asks it of two different things — the answers disagree, and neither is
 * the truth about that locator. Staying silent about it is the same honesty
 * that keeps an incomplete object from being blamed; the day a copy receives
 * types of its own, the two answers will belong to two locators and the
 * disagreement will disappear on its own.</p>
 *
 * <p>Only the last draining says what is missing. An earlier one judges with
 * less than everything known, and a mistake found there may be no mistake at
 * all once the last fact arrives, so its verdicts are thrown away and the
 * final pass, which knows everything the loop can know, writes the table.</p>
 *
 * @since 0.68.0
 */
final class Settled {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * The needs table.
     */
    private final XML wanted;

    /**
     * The checks table.
     */
    private final XML pending;

    /**
     * The name every type goes by, from {@link Same}.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     * @param provides The provides table
     * @param needs The needs table
     * @param checks The checks table
     * @param aliases The name every type goes by, from {@link Same}
     */
    Settled(
        final XML provides,
        final XML needs,
        final XML checks,
        final Map<String, String> aliases
    ) {
        this.given = provides;
        this.wanted = needs;
        this.pending = checks;
        this.names = aliases;
    }

    /**
     * Drain the checks into the given table, a row per mistake, and hand back
     * what the loop worked out about the objects nobody wrote down.
     * @param rows The table to write the mistakes into
     * @return What every computed object is a copy of, by its locator
     * @throws IOException If the rows of a draining cannot be let go of
     */
    Map<String, String> drain(final Tojos rows) throws IOException {
        final Map<String, Collection<String>> answers = new HashMap<>(0);
        boolean fresh = true;
        while (fresh) {
            fresh = false;
            try (Tojos ignored = new TjDeferred(new MnMemory())) {
                for (final Map.Entry<String, Collection<String>> found
                    : this.drained(answers, ignored).entrySet()) {
                    fresh = answers.computeIfAbsent(
                        found.getKey(), made -> new HashSet<>(1)
                    ).addAll(found.getValue()) || fresh;
                }
            }
        }
        this.drained(answers, rows);
        return new Agreed(answers).names();
    }

    /**
     * Drain the checks once, knowing what has been learned so far.
     * @param answers What every computed object has come out as, so far
     * @param rows The table to write the mistakes into
     * @return What this draining worked out
     */
    private Map<String, Collection<String>> drained(
        final Map<String, Collection<String>> answers, final Tojos rows
    ) {
        final Map<String, String> known = new HashMap<>(this.names);
        known.putAll(new Agreed(answers).names());
        final Map<String, String> ends = new Ends(known).names();
        return new Worklist(
            ends,
            new Provided(new Ungrouped(this.given, ends).rows()),
            new Ungrouped(this.wanted, ends).rows(),
            new Ungrouped(this.pending, ends).rows()
        ).drain(rows);
    }
}
