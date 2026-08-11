/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.tojos.Tojos;
import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * The checks, drained one by one.
 *
 * <p>{@link Checks} filed a promise for every argument put into a copy of
 * something: this argument has to fit into that void. Deciding one is what the
 * other three tables are for. The argument is a copy of an object
 * {@link Provides} has written down, the void is asked for attributes
 * {@link Needs} has written down, and {@link Links} says which of those types
 * are the same thing. So a check is decided by asking the object for
 * everything the void is asked for.</p>
 *
 * <p>"Yes, it has it" is rarely the end of it: whatever the attribute holds
 * must in turn have everything that taking it is asked for, which is a smaller
 * check of exactly the same shape. Those go back into the list, and the list is
 * drained until nothing is left in it. On the program of the design note that
 * takes the three steps the note describes: the {@code t} fills the void
 * {@code x}, the {@code x} is asked for {@code next}, and the {@code next} it
 * finds is asked for {@code foo}.</p>
 *
 * <p>A mistake is written down only when the checker is sure: the object that
 * misses the attribute has been seen whole, and the attribute is still not
 * there. An atom, an object nobody in the program describes, an argument whose
 * void cannot be found — all of these leave a check undecided and nothing is
 * said about them. It is better to miss a mistake than to complain about
 * correct code.</p>
 *
 * <p>A check already started is dropped rather than started again. Nothing new
 * is learned while the list is drained, since all four tables were written
 * before it started, so a check nobody can decide now will not become
 * decidable later: the list only ever grows by splitting a check into smaller
 * ones, of which there are finitely many. Remembering the ones already started
 * is what keeps an object that refers to itself from being walked forever.</p>
 *
 * @since 0.68.0
 */
final class Worklist {

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * What the types certainly have.
     */
    private final Provided given;

    /**
     * The rows of the needs table, by the name their owner goes by.
     */
    private final Map<String, Collection<Map<String, String>>> wanted;

    /**
     * The rows of the checks table, by the name their owner goes by.
     */
    private final Map<String, Collection<Map<String, String>>> pending;

    /**
     * Ctor.
     * @param aliases The name every type goes by, from {@link Same}
     * @param provided What the types certainly have
     * @param needs The rows of the needs table, by the name of their owner
     * @param checks The rows of the checks table, by the name of their owner
     */
    Worklist(
        final Map<String, String> aliases,
        final Provided provided,
        final Map<String, Collection<Map<String, String>>> needs,
        final Map<String, Collection<Map<String, String>>> checks
    ) {
        this.names = aliases;
        this.given = provided;
        this.wanted = needs;
        this.pending = checks;
    }

    /**
     * Drain the checks into the given table, a row per mistake.
     * @param rows The table to write the mistakes into
     */
    void drain(final Tojos rows) {
        final Deque<String> todo = new ArrayDeque<>(0);
        for (final Map.Entry<String, Collection<Map<String, String>>> copy
            : this.pending.entrySet()) {
            for (final Map<String, String> argument : copy.getValue()) {
                if (argument.containsKey("name")) {
                    final String hole = this.hole(copy.getKey(), argument.get("name"));
                    if (!hole.isEmpty()) {
                        todo.add(
                            String.join(
                                " ", this.name(argument.getOrDefault("type", "")), hole
                            )
                        );
                    }
                }
            }
        }
        final Collection<String> started = new HashSet<>(0);
        while (!todo.isEmpty()) {
            final String check = todo.poll();
            if (started.add(check)) {
                this.decide(check, todo, rows);
            }
        }
    }

    /**
     * Decide one check: has the object everything the void is asked for?
     * @param check The name of the object and the name of the void, together
     * @param todo The list to put the smaller checks into
     * @param rows The table to write a mistake into, if there is one
     */
    private void decide(final String check, final Collection<String> todo, final Tojos rows) {
        final String has = check.substring(0, check.indexOf(' '));
        final String want = check.substring(check.indexOf(' ') + 1);
        for (final Map<String, String> need
            : this.wanted.getOrDefault(want, Collections.emptyList())) {
            if (need.containsKey("name")) {
                final String owned = this.given.attribute(has, need.get("name"));
                if (owned.isEmpty() && this.given.complete(has)) {
                    rows.add(has);
                    rows.add(String.join(" ", has, need.get("name")))
                        .set("owner", has)
                        .set("name", need.get("name"))
                        .set("asked", need.getOrDefault("type", ""));
                }
                if (!owned.isEmpty()) {
                    todo.add(
                        String.join(" ", this.name(owned), need.getOrDefault("type", ""))
                    );
                }
            }
        }
    }

    /**
     * The void an argument bound under the given name lands in.
     * @param copied The name of the object being copied
     * @param name The name the argument is bound under
     * @return The type of the void, or an empty string when it cannot be
     *  found, which is the answer for anything the tables do not describe
     */
    private String hole(final String copied, final String name) {
        final String found;
        if (name.startsWith("α")) {
            final List<String> holes = this.given.voids(copied);
            final int place = Integer.parseInt(name.substring(1));
            if (place < holes.size()) {
                found = holes.get(place);
            } else {
                found = "";
            }
        } else {
            found = this.given.attribute(copied, name);
        }
        return found;
    }

    /**
     * The name the given type goes by.
     * @param type The type
     * @return The name, which is the type itself when it is a copy of nothing
     */
    private String name(final String type) {
        return this.names.getOrDefault(type, type);
    }
}
