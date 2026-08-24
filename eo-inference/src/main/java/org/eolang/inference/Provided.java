/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
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
 * kinds the same way. And whatever the type hands its answers to, which is
 * what stands behind its {@code φ}, or, for an atom, the type it says it comes
 * back with.</p>
 *
 * <p>The walk stops at a type it has already passed, so an object that
 * delegates in a circle is walked once and answers nothing. A void answers
 * too, but with the name rooted at itself: the table has no row for one,
 * since what a void holds is decided by whoever fills it, so the answer
 * holds for every caller and is concrete for none — unless the source has
 * said what will go in, which a formation only Java ever copies has to do
 * (#6189). Such a void is walked through like a delegation instead, and the
 * answer is the same for every caller, which is what an annotation
 * claims.</p>
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
     * The locator of every void, from {@link Hollows}.
     */
    private final Collection<String> hollows;

    /**
     * What every void that says so will hold, from {@link Held}.
     */
    private final Map<String, String> held;

    /**
     * Ctor.
     * @param provides The provides table, as {@link Provides} wrote it
     * @param aliases The name every type goes by, from {@link Same}
     * @param voids The locator of every void, from {@link Hollows}
     */
    Provided(
        final XML provides,
        final Map<String, String> aliases,
        final Collection<String> voids
    ) {
        this(
            new Ungrouped(provides, aliases).rows(), aliases, voids,
            new Held(provides).all()
        );
    }

    /**
     * Ctor.
     * @param rows The rows of the provides table, by the name of their owner
     * @param aliases The name every type goes by, from {@link Same}
     * @param voids The locator of every void, from {@link Hollows}
     * @param holds What every void that says so will hold, from {@link Held}
     */
    Provided(
        final Map<String, Collection<Map<String, String>>> rows,
        final Map<String, String> aliases,
        final Collection<String> voids,
        final Map<String, String> holds
    ) {
        this.table = rows;
        this.names = aliases;
        this.hollows = voids;
        this.held = holds;
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
     * The void this type keeps in the given place.
     * @param type The name the type goes by
     * @param place The place of the void among the voids of this type
     * @return The locator of the void, or an empty string when this type keeps
     *  fewer voids than that
     */
    String slot(final String type, final int place) {
        String found = "";
        int seen = 0;
        for (final Map<String, String> row : this.own(type)) {
            if ("true".equals(row.get("void")) && !"ρ".equals(row.get("name"))) {
                if (seen == place) {
                    found = row.getOrDefault("type", "");
                    break;
                }
                seen += 1;
            }
        }
        return found;
    }

    /**
     * The void this type declares for the object a dispatch takes it from.
     *
     * <p>Since #6657 the receiver is a void like any other, named {@code ρ}
     * and written down where the formation wants it, so it is filled by
     * whoever dispatches into the type and never by an argument counted from
     * the left. That is why {@link #slot(String, int)} steps over it.</p>
     *
     * @param type The name the type goes by
     * @return The locator of the void, or an empty string when this type
     *  declares none
     */
    String receiver(final String type) {
        String found = "";
        for (final Map<String, String> row : this.own(type)) {
            if ("true".equals(row.get("void")) && "ρ".equals(row.get("name"))) {
                found = row.getOrDefault("type", "");
                break;
            }
        }
        return found;
    }

    private boolean hollow(final String type) {
        boolean found = false;
        String walked = type;
        while (!walked.isEmpty()) {
            if (this.hollows.contains(walked)) {
                found = true;
                break;
            }
            if (!walked.contains(".")) {
                break;
            }
            walked = walked.substring(0, walked.lastIndexOf('.'));
        }
        return found;
    }

    private String kept(final String type, final String name, final Collection<String> walked) {
        String found = this.bound(type, name);
        final String member = String.join(".", type, name);
        if (found.isEmpty() && (this.table.containsKey(member) || this.blank(type))) {
            found = member;
        }
        final String behind = this.behind(type);
        if (found.isEmpty() && !behind.isEmpty() && walked.add(type)) {
            found = this.kept(behind, name, walked);
        }
        return found;
    }

    private String behind(final String type) {
        String next = this.bound(type, "φ");
        if (next.isEmpty()) {
            next = this.cell(type, "returns");
        }
        if (next.isEmpty()) {
            next = this.held.getOrDefault(type, "");
        }
        return this.names.getOrDefault(next, next);
    }

    private boolean blank(final String type) {
        return this.hollow(type) && !this.held.containsKey(type);
    }

    private String cell(final String type, final String cell) {
        String found = "";
        for (final Map<String, String> row : this.own(type)) {
            if (row.containsKey("id")) {
                found = row.getOrDefault(cell, "");
            }
        }
        return found;
    }

    private String bound(final String type, final String name) {
        String found = "";
        for (final Map<String, String> row : this.own(type)) {
            if (name.equals(row.getOrDefault("name", ""))) {
                found = row.getOrDefault("type", "");
            }
        }
        return found;
    }

    private Collection<Map<String, String>> own(final String type) {
        return this.table.getOrDefault(type, Collections.emptyList());
    }
}
