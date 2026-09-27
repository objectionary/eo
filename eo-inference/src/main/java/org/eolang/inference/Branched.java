/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.function.Predicate;

/**
 * What a call hands back, where every formation it reaches hands back what the
 * call put into it.
 *
 * <p>A formation whose whole body is one of its own voids gives back whatever
 * was put there and nothing of its own: the {@code [? >> left ? >> right]}
 * that {@code Φ.true} hands to {@code Φ.bool.if} answers with its {@code left}
 * and the one {@code Φ.false} hands answers with its {@code right}. So a call
 * on that void is one of the two arguments, and which one is not known —
 * whichever it is, it is what they both are.</p>
 *
 * <p>Nothing is guessed, so one formation that binds a body of its own is the
 * end of it: the call may be that one, what comes back is then that body, and
 * a body is a question for whoever walks a delegation and not for this. Where
 * a void holds a formation of each kind there is no agreement to join, and the
 * call is left rooted at the void it was. A void does go by a name of its own
 * once one thing has been seen in it, though, so the body that was
 * {@code left} yesterday is a {@code Φ.dial} today and reads like a body the
 * formation binds. What the call put in says which it is: a body that is one
 * of the arguments is a void wearing the argument's name.</p>
 *
 * <p>An arm rooted at a void this call leaves empty is left out of the
 * agreement. Reading a void nobody filled terminates, so that arm never hands
 * a value to anyone, and a caller holding one got it from another arm. Every
 * fragile object is written this way, with the excuse in one arm and the
 * answer in the other, and the callers who want the answer fill nothing.</p>
 *
 * <p>Left out of the agreement, though, and not out of the choice. A void this
 * call leaves empty is one another call of the same object fills, and then the
 * arm does hand a value to somebody. Dropping it left one arm standing and the
 * agreement was that arm, which is a lie told about every caller who took the
 * other one (#8875). So the whole of what the call may come back with counts
 * an arm as long as anybody fills the void it reads, and only an arm nobody
 * anywhere fills is gone for good.</p>
 *
 * <p>An arm that terminates is gone from both. It is not rooted at a void, so
 * nothing above says it hands nothing back, and yet it never does: the
 * {@code tmpfile} of a {@code directory} is a {@code Φ.file} in one arm and an
 * error in the other, and no caller ever holds the error. Left in, it agrees
 * with nothing, and the call was left rooted at the void it was (#8946).</p>
 *
 * @since 0.71.0
 */
final class Branched {

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * What the call put into the voids, by the locator of the void.
     */
    private final Map<String, String> binds;

    /**
     * The locator of every void.
     */
    private final Collection<String> hollows;

    /**
     * What the calls of the program put into its voids.
     */
    private final Puts every;

    /**
     * Ctor.
     *
     * @param provided What the types certainly have
     * @param filled What the call put into the voids, by the locator of the
     *  void
     * @param voids The locator of every void, from {@link Hollows}
     * @param puts What the calls of the program put into its voids
     */
    Branched(
        final Provided provided,
        final Map<String, String> filled,
        final Collection<String> voids,
        final Puts puts
    ) {
        this.owned = provided;
        this.binds = filled;
        this.hollows = voids;
        this.every = puts;
    }

    /**
     * The one thing every formation this call reaches hands back.
     *
     * @return The locator, empty when no formation hands back what it was
     *  given or they share nothing
     */
    String names() {
        return new Joined(this.arms(), this.owned).names();
    }

    /**
     * What the formations this call reaches hand back, one apiece.
     *
     * <p>Where they agree this is the agreement said the long way round, and
     * where they do not it is the whole of what the call may come back with:
     * a choice between the arms, which is an answer of its own for whoever can
     * hold two of them (#8744).</p>
     *
     * @return The locators, empty when a formation this call reaches binds a
     *  body of its own
     */
    Collection<String> arms() {
        return this.handed(arm -> false);
    }

    /**
     * The whole of what the call may come back with, one arm apiece.
     *
     * <p>Unlike {@link #arms()}, an arm rooted at a void this call leaves empty
     * is kept, as long as some call of the program fills it. Nothing about this
     * call says which arm it comes back with, and an arm another caller gets a
     * value out of is one this caller may get a value out of too (#8875).</p>
     *
     * @return The locators, empty when a formation this call reaches binds a
     *  body of its own
     */
    Collection<String> whole() {
        return this.handed(this::filled);
    }

    private Collection<String> handed(final Predicate<String> alive) {
        final Collection<String> handed = new LinkedHashSet<>(0);
        for (final Map.Entry<String, Map<String, String>> owner : this.owners().entrySet()) {
            final Collection<String> given = this.given(owner.getKey(), owner.getValue());
            if (given.isEmpty()) {
                handed.clear();
                break;
            }
            given.removeIf(arm -> !this.stands(arm) && !alive.test(arm));
            given.removeIf(this.every::dies);
            handed.addAll(given);
        }
        return handed;
    }

    private boolean filled(final String arm) {
        return this.every.fills(new Rooted(this.hollows).names(arm));
    }

    private Map<String, Map<String, String>> owners() {
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, String> bind : this.binds.entrySet()) {
            final int dot = bind.getKey().lastIndexOf('.');
            if (dot > 0) {
                found.computeIfAbsent(
                    bind.getKey().substring(0, dot), key -> new LinkedHashMap<>(1)
                ).put(bind.getKey(), bind.getValue());
            }
        }
        return found;
    }

    private Collection<String> given(final String owner, final Map<String, String> arms) {
        final Collection<String> found = new LinkedHashSet<>(0);
        for (final Map.Entry<String, String> arm : arms.entrySet()) {
            if (this.hands(owner, arm)) {
                found.add(arm.getValue());
            }
        }
        return found;
    }

    private boolean hands(final String owner, final Map.Entry<String, String> bind) {
        final String body = this.owned.behind(owner);
        return body.equals(bind.getKey()) || body.equals(bind.getValue());
    }

    private boolean stands(final String arm) {
        final String root = new Rooted(this.hollows).names(arm);
        return root.isEmpty() || this.binds.containsKey(root);
    }
}
