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
 * <p>A formation that binds a body of its own among them is an arm as well,
 * and the arm is that body, whatever went into the slots: the {@code pick} of
 * an {@code odd} whose body is {@code 42} makes a call that holds it one of
 * the arguments or a {@code Φ.number}. It used to be the end of it, and one
 * such formation threw away the arms every other one had handed back, so the
 * call was left rooted at the void it was (#8980). Nothing is guessed: the arm
 * is the name the body goes by, and a body nobody has settled yet goes by its
 * own locator, which is a member of the choice that says nothing beyond
 * itself until a later pass settles it. Where no formation the void holds
 * hands back what the call put in, there is no choice to join, since then the
 * call is a copy of what the void holds and says so itself: a void that holds
 * the {@code odd} alone makes the call a copy of its {@code pick}, and
 * {@link Behaved} reduces that copy to the {@code Φ.number} of its body. A void
 * does go by a name of its own once one thing has been seen in it, though,
 * so the body that was {@code left} yesterday is a {@code Φ.dial} today and
 * reads like a body the formation binds. What the call put in says which it
 * is: a body that is one of the arguments is a void wearing the argument's
 * name.</p>
 *
 * <p>An arm rooted at a void this call leaves empty counts like any other.
 * The call does not fill that void, but whoever called the object the arm
 * sits in did: the {@code made} of a {@code directory} hands back its own
 * receiver in one arm and a {@code seq} in the other, and the {@code if} that
 * chooses between them fills neither the receiver nor what the first arm asks
 * of it. Dropping that arm left the {@code seq} standing and the agreement was
 * the {@code seq}, which is a lie told about every caller who got the
 * directory back. So the arm stays in the agreement and in the choice alike,
 * and where it shares nothing with the other arms the call is a choice of all
 * of them (#8885).</p>
 *
 * <p>It is dropped where nobody in the program fills that void. The program
 * is everything compiled together, so there is no later caller to fill it,
 * and a run that takes the arm reads an empty void and stops there: the
 * {@code cant-read} of a {@code gauge} that no call passes is as dead as an
 * arm that terminates. A {@code ρ} and a void that says what it holds are
 * never empty, since whoever dispatches fills the one and the other is true
 * of every caller (#8981).</p>
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
     * @return The locators, empty when no formation this call reaches hands
     *  back what it was given
     */
    Collection<String> arms() {
        final Map<String, Collection<String>> given = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Map<String, String>> owner : this.owners().entrySet()) {
            given.put(owner.getKey(), this.given(owner.getKey(), owner.getValue()));
        }
        final Collection<String> handed = new LinkedHashSet<>(0);
        if (given.values().stream().anyMatch(arm -> !arm.isEmpty())) {
            for (final Map.Entry<String, Collection<String>> arm : given.entrySet()) {
                if (arm.getValue().isEmpty()) {
                    arm.getValue().add(this.owned.behind(arm.getKey()));
                }
                if (arm.getValue().contains("")) {
                    handed.clear();
                    break;
                }
                arm.getValue().removeIf(this.every::dies);
                arm.getValue().removeIf(this::vacant);
                handed.addAll(arm.getValue());
            }
        }
        return handed;
    }

    private boolean vacant(final String filling) {
        final String hollow = this.owned.rooted(filling);
        return !hollow.isEmpty() && !this.every.fills(hollow);
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
