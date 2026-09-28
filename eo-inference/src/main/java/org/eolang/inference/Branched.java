/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

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
 * <p>An arm rooted at a void this call leaves empty counts like any other.
 * The call does not fill that void, but whoever called the object the arm
 * sits in did: the {@code made} of a {@code directory} hands back its own
 * receiver in one arm and a {@code seq} in the other, and the {@code if} that
 * chooses between them fills neither the receiver nor what the first arm asks
 * of it. Dropping that arm left the {@code seq} standing and the agreement was
 * the {@code seq}, which is a lie told about every caller who got the
 * directory back. Nor is it dropped where nobody in the program fills the
 * void, since the program is not every caller there will be. So the arm stays
 * in the agreement and in the choice alike, and where it shares nothing with
 * the other arms the call is a choice of all of them (#8885).</p>
 *
 * <p>An arm that terminates is gone from both, since it never hands a value
 * back: the {@code tmpfile} of a {@code directory} is a {@code Φ.file} in one
 * arm and an error in the other, and no caller ever holds the error. Left in,
 * it agrees with nothing, and the call was left rooted at the void it was
 * (#8946).</p>
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
     * What the calls of the program put into its voids.
     */
    private final Puts every;

    /**
     * Ctor.
     *
     * @param provided What the types certainly have
     * @param filled What the call put into the voids, by the locator of the
     *  void
     * @param puts What the calls of the program put into its voids
     */
    Branched(final Provided provided, final Map<String, String> filled, final Puts puts) {
        this.owned = provided;
        this.binds = filled;
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
        final Collection<String> handed = new LinkedHashSet<>(0);
        for (final Map.Entry<String, Map<String, String>> owner : this.owners().entrySet()) {
            final Collection<String> given = this.given(owner.getKey(), owner.getValue());
            if (given.isEmpty()) {
                handed.clear();
                break;
            }
            given.removeIf(this.every::dies);
            handed.addAll(given);
        }
        return handed;
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
}
