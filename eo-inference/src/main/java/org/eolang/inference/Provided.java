/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;

/**
 * What the types of a program certainly have.
 *
 * <p>This is the table {@link Provides} wrote, read by the name a type and
 * its copies go by, so that a question about a copy is answered by the
 * formation it is a copy of. One question is asked of it: what the type of an
 * attribute is, or nothing at all when the type has no such attribute.</p>
 *
 * <p>An attribute is looked for in three places, because there are three. The
 * type itself. Its package, since an attribute nobody binds falls
 * through to the object of that name beside it — {@code Φ.number} binds eight
 * attributes and answers to forty, the rest of them being objects of their own
 * in the same package, {@code Φ.number.eq} among them, and a locator names both
 * kinds the same way. And whatever stands behind its {@code φ}, which answers
 * for every name the object does not bind itself.</p>
 *
 * <p>The walk stops at a type it has already passed, so an object that
 * delegates in a circle is walked once and answers nothing. A void answers
 * nothing either: the table has no row for one, since what a void holds is
 * decided by whoever fills it.</p>
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
