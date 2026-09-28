/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The arms of every choice, each with the receiver it is read off.
 *
 * <p>An attribute read off an object gets that object as its receiver, and a
 * row says so with a bind on the {@code ρ} of whatever the attribute is a
 * copy of. A row that comes back with one of several objects said nothing of
 * the kind, since its pair is a name rooted at a void and has no receiver to
 * give. {@code p.ok}, where {@code p} is a {@code gauge} that chooses between
 * a {@code record} and whatever its {@code cant-read} holds, is the
 * {@code ok} of either, and in the first arm that {@code ok} is a copy of a
 * {@code lamp} and hangs off the {@code record}. Nothing told the
 * {@code lamp}, which then heard only of its other callers and took a census
 * of one for the type of its receiver (#8885).</p>
 *
 * <p>So every arm is walked down its chain of copies the way {@link Stamped}
 * walks a pair, and where the walk ends on an object that declares a
 * receiver, the arm carries a bind saying the object it was read off went
 * there. That object is the one the arm is an attribute of, and not the
 * receiver written before the dot: the {@code ok} is not the {@code gauge}'s
 * own, it is the {@code record}'s, found behind the {@code φ} of the gauge.
 * An arm that is no attribute of a formation carries nothing: an argument
 * handed to the call, which a picker hands back as it got it, a void, which
 * holds what a caller wrote elsewhere and got its receiver there, such as
 * the {@code from} of a regex {@code span}, an attribute of a void, whose
 * owner is not known, and an object of the root, such as a {@code false}
 * written by itself, which hangs off no copy at all.</p>
 *
 * <p>The bind stays inside its arm, since two arms may give the same
 * receiver two different objects, and one row can hold only one of them.</p>
 *
 * @since 0.76.0
 */
final class Forked {

    /**
     * What every call on a void may come back with, from {@link Dispatched}.
     */
    private final Map<String, Collection<String>> arms;

    /**
     * Where a receiver lands down the chain of copies.
     */
    private final Stamped stamped;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * The locator of every void, from {@link Hollows}.
     */
    private final Collection<String> hollows;

    /**
     * Ctor.
     *
     * @param chosen What every call on a void may come back with
     * @param lands Where a receiver lands down the chain of copies
     * @param provided What the types certainly have
     * @param voids The locator of every void
     */
    Forked(
        final Map<String, Collection<String>> chosen, final Stamped lands,
        final Provided provided, final Collection<String> voids
    ) {
        this.arms = chosen;
        this.stamped = lands;
        this.owned = provided;
        this.hollows = voids;
    }

    /**
     * The arms of every choice, each with what it puts into a receiver.
     *
     * @return The arms, by the locator of the object the row is about
     */
    Map<String, Collection<Type>> all() {
        final Map<String, Collection<Type>> found = new LinkedHashMap<>(this.arms.size());
        for (final Map.Entry<String, Collection<String>> choice : this.arms.entrySet()) {
            final Collection<Type> members = new ArrayList<>(choice.getValue().size());
            for (final String arm : choice.getValue()) {
                members.add(new Ref(arm, this.binds(arm)));
            }
            found.put(choice.getKey(), members);
        }
        return found;
    }

    private Map<String, String> binds(final String arm) {
        final int dot = arm.lastIndexOf('.');
        final Map<String, String> found;
        if (dot == arm.indexOf('.') || arm.startsWith("α", dot + 1)
            || new Rooted(this.hollows).covers(arm)
            || !arm.equals(this.owned.here(arm.substring(0, dot), arm.substring(dot + 1)))) {
            found = Collections.emptyMap();
        } else {
            final String hollow = this.owned.receiver(this.stamped.lands(arm));
            if (hollow.isEmpty()) {
                found = Collections.emptyMap();
            } else {
                found = Collections.singletonMap(hollow, arm.substring(0, dot));
            }
        }
        return found;
    }
}
