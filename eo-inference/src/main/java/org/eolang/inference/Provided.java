/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
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
 * <p>Whether the whole of a type has been seen, which is what makes a missing
 * attribute a mistake rather than a gap. A type is whole when it says so and
 * nothing that goes by the same name says otherwise: an argument put into a
 * copy of an atom is as unknown as the atom.</p>
 *
 * <p>What the type of an attribute is, or nothing at all when the type has no
 * such attribute. An attribute is looked for in two places, because there are
 * two: the type itself, and its package, since an attribute nobody binds falls
 * through to the object of that name beside it — {@code Φ.number} binds eight
 * attributes and answers to forty, the rest of them being objects of their own
 * in the same package, {@code Φ.number.eq} among them. A locator names both
 * kinds the same way, so the second place costs one lookup. Delegation through
 * {@code φ} is a third place, and is not looked into yet.</p>
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
     * Ctor.
     * @param rows The rows of the provides table, by the name of their owner
     */
    Provided(final Map<String, Collection<Map<String, String>>> rows) {
        this.table = rows;
    }

    /**
     * Has the whole of this type been seen?
     * @param type The name the type goes by
     * @return TRUE when nothing about the type is hidden from the checker
     */
    boolean complete(final String type) {
        final Collection<Boolean> flags = new ArrayList<>(1);
        for (final Map<String, String> row : this.own(type)) {
            if (row.containsKey("complete")) {
                flags.add(Boolean.parseBoolean(row.get("complete")));
            }
        }
        return !flags.isEmpty() && !flags.contains(false);
    }

    /**
     * The type of the attribute this type keeps under the given name.
     * @param type The name the type goes by
     * @param name The name of the attribute
     * @return The type of the attribute, or an empty string when this type has
     *  no attribute of that name
     */
    String attribute(final String type, final String name) {
        String found = "";
        for (final Map<String, String> row : this.own(type)) {
            if (name.equals(row.getOrDefault("name", ""))) {
                found = row.getOrDefault("type", "");
            }
        }
        final String member = String.join(".", type, name);
        if (found.isEmpty() && this.table.containsKey(member)) {
            found = member;
        }
        return found;
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
     * The rows about the type of the given name.
     * @param type The name the type goes by
     * @return The rows, empty when the table says nothing about it
     */
    private Collection<Map<String, String>> own(final String type) {
        return this.table.getOrDefault(type, Collections.emptyList());
    }
}
