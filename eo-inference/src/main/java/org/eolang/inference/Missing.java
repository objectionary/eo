/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.tojos.Tojos;
import java.util.Collection;
import java.util.Map;

/**
 * What a program asks for and will never find.
 *
 * <p>{@link Needs} has a row for every dispatch, saying which object is asked
 * for which name; {@link Links}, closed by {@link Resolved}, says what that
 * object is; and {@link Provides} says what it holds. So a mistake is those
 * three read together, and nothing else is needed to find one: the object
 * {@code .foo} is taken from has been seen whole, and there is no {@code foo}
 * in it.</p>
 *
 * <p>Both halves of that sentence are guards. "Seen whole" leaves an atom, an
 * object nobody in the program describes, and anything a caller decides out of
 * the verdict — a name taken from a void is a question, not a claim, and the
 * table has no row for one, so it is never whole. "There is no such name"
 * means there is none in the type, none in its package, and none behind what
 * it delegates to, which {@link Provided} walks before answering. It is better
 * to miss a mistake than to complain about correct code.</p>
 *
 * <p>What that leaves out is the mistake that is one for a single caller. In
 * the program of the design note, {@code inc} takes {@code foo} from whatever
 * it is given, and it is given a {@code t} whose {@code next} has no
 * {@code foo}; the dispatch itself is written against a void and is right for
 * some other argument, so nothing is said about it. Only where a receiver is
 * concrete where it stands is anything said at all, which is why the verdicts
 * hold for every caller of the code they are about.</p>
 *
 * @since 0.69.0
 */
final class Missing {

    /**
     * What the types certainly have.
     */
    private final Provided given;

    /**
     * The rows of the needs table, by the name their owner goes by.
     */
    private final Map<String, Collection<Map<String, String>>> wanted;

    /**
     * Ctor.
     * @param provided What the types certainly have
     * @param needs The rows of the needs table, by the name of their owner
     */
    Missing(
        final Provided provided,
        final Map<String, Collection<Map<String, String>>> needs
    ) {
        this.given = provided;
        this.wanted = needs;
    }

    /**
     * Write every mistake into the given table.
     * @param rows The table to fill
     */
    void fill(final Tojos rows) {
        for (final Map.Entry<String, Collection<Map<String, String>>> asked
            : this.wanted.entrySet()) {
            if (this.given.complete(asked.getKey())) {
                for (final Map<String, String> need : asked.getValue()) {
                    this.judge(asked.getKey(), need, rows);
                }
            }
        }
    }

    /**
     * Write this one question down, when the type will never answer it.
     *
     * <p>A row is kept under the locator of the dispatch, since the same name
     * may be taken from the same object in several places and every one of
     * them is a mistake of its own, worth pointing at.</p>
     *
     * @param type The name the type goes by
     * @param need The row of the needs table about the question
     * @param rows The table to fill
     */
    private void judge(
        final String type, final Map<String, String> need, final Tojos rows
    ) {
        final String name = need.getOrDefault("name", "");
        if (!name.isEmpty() && this.given.attribute(type, name).isEmpty()) {
            final String where = need.getOrDefault("type", "");
            rows.add(type);
            rows.add(where).set("owner", type).set("name", name).set("asked", where);
        }
    }
}
