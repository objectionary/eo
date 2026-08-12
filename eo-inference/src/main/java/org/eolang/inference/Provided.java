/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * What the types of a program certainly have.
 *
 * <p>This is the table {@link Provides} wrote, read by the name a type and
 * its copies go by, so that a question about an argument is answered by the
 * formation the argument is a copy of. Three questions are asked of it while
 * the checks are drained.</p>
 *
 * <p>What the type of an attribute is, or nothing at all when the type has no
 * such attribute. An attribute is looked for in three places, because there are
 * three. The type itself. Its package, since an attribute nobody binds falls
 * through to the object of that name beside it — {@code Φ.number} binds eight
 * attributes and answers to forty, the rest of them being objects of their own
 * in the same package, {@code Φ.number.eq} among them, and a locator names both
 * kinds the same way. And whatever stands behind its {@code φ}, which answers
 * for every name the object does not bind itself.</p>
 *
 * <p>Whether the whole of a type has been seen, which is what makes a missing
 * attribute a mistake rather than a gap. A type is whole when it says so and
 * nothing that goes by the same name says otherwise, or when it delegates and
 * the object it delegates to is whole. The chain is what carries the doubt:
 * an argument put into a copy of an atom is as unknown as the atom, and so is
 * anything that hands its answers to one. Since no formation in the runtime
 * binds both {@code λ} and {@code φ}, following the decoratee cannot walk past
 * a body written in Java.</p>
 *
 * <p>Both walks stop at a type they have already passed, so an object that
 * delegates in a circle is walked once and answers nothing.</p>
 *
 * <p>And which of its attributes are void, in the order they were declared,
 * because an argument fills a void by its place among them.</p>
 *
 * @since 0.68.0
 */
final class Provided {

    /**
     * The rows of the provides table, by the name their owner goes by.
     */
    private final Map<String, Collection<Map<String, String>>> table;

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     * @param rows The rows of the provides table, by the name of their owner
     * @param aliases The name every type goes by, from {@link Same}
     */
    Provided(
        final Map<String, Collection<Map<String, String>>> rows,
        final Map<String, String> aliases
    ) {
        this.table = rows;
        this.names = aliases;
    }

    /**
     * Has the whole of this type been seen?
     * @param type The name the type goes by
     * @return TRUE when nothing about the type is hidden from the checker
     */
    boolean complete(final String type) {
        return this.whole(type, new HashSet<>(0));
    }

    /**
     * The type of the attribute this type keeps under the given name.
     * @param type The name the type goes by
     * @param name The name of the attribute
     * @return The type of the attribute, or an empty string when this type has
     *  no attribute of that name
     */
    String attribute(final String type, final String name) {
        return this.kept(type, name, new HashSet<>(0));
    }

    /**
     * The types of the void attributes of this type, in the order they were
     * declared.
     * @param type The name the type goes by
     * @return The types, empty when the type declares no voids
     */
    List<String> voids(final String type) {
        final List<String> found = new ArrayList<>(0);
        for (final Map<String, String> row : this.own(type)) {
            if (row.containsKey("void")) {
                found.add(row.getOrDefault("type", ""));
            }
        }
        return found;
    }

    /**
     * Has the whole of this type been seen, following what it delegates to?
     * @param type The name the type goes by
     * @param walked The types passed already
     * @return TRUE when nothing about the type is hidden from the checker
     */
    private boolean whole(final String type, final Collection<String> walked) {
        final Collection<Boolean> flags = new ArrayList<>(1);
        for (final Map<String, String> row : this.own(type)) {
            if (row.containsKey("complete")) {
                flags.add(Boolean.parseBoolean(row.get("complete")));
            }
        }
        boolean found = !flags.isEmpty() && !flags.contains(false);
        final String behind = this.behind(type);
        if (!found && !behind.isEmpty() && walked.add(type)) {
            found = this.whole(behind, walked);
        }
        return found;
    }

    /**
     * The type of the attribute this type keeps, looking behind what it
     * delegates to when it keeps none.
     * @param type The name the type goes by
     * @param name The name of the attribute
     * @param walked The types passed already
     * @return The type of the attribute, or an empty string
     */
    private String kept(final String type, final String name, final Collection<String> walked) {
        String found = this.bound(type, name);
        final String member = String.join(".", type, name);
        if (found.isEmpty() && this.table.containsKey(member)) {
            found = member;
        }
        final String behind = this.behind(type);
        if (found.isEmpty() && !behind.isEmpty() && walked.add(type)) {
            found = this.kept(behind, name, walked);
        }
        return found;
    }

    /**
     * The type this one hands its answers to.
     * @param type The name the type goes by
     * @return The name of the type behind its {@code φ}, or an empty string
     *  when the type binds no {@code φ} and answers for itself
     */
    private String behind(final String type) {
        final String decoratee = this.bound(type, "φ");
        return this.names.getOrDefault(decoratee, decoratee);
    }

    /**
     * The type of the attribute this type binds itself.
     * @param type The name the type goes by
     * @param name The name of the attribute
     * @return The type of the attribute, or an empty string
     */
    private String bound(final String type, final String name) {
        String found = "";
        for (final Map<String, String> row : this.own(type)) {
            if (name.equals(row.getOrDefault("name", ""))) {
                found = row.getOrDefault("type", "");
            }
        }
        return found;
    }

    /**
     * The rows about the type of the given name.
     * @param type The name the type goes by
     * @return The rows, empty when the table says nothing about it
     */
    private Collection<Map<String, String>> own(final String type) {
        return this.table.getOrDefault(type, Collections.emptyList());
    }
}
