/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
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
 * <p>Nothing is guessed, since a formation that binds a body of its own is
 * passed over: the call goes into it and what comes back is that body, which
 * is a question for whoever walks a delegation and not for this. A void does
 * go by a name of its own once one thing has been seen in it, though, so the
 * body that was {@code left} yesterday is a {@code Φ.dial} today and reads
 * like a body the formation binds. What the call put in says which it is: a
 * body that is one of the arguments is a void wearing the argument's name.</p>
 *
 * <p>An arm rooted at a void this call leaves empty is left out of the
 * agreement. Reading a void nobody filled terminates, so that arm never hands
 * a value to anyone, and a caller holding one got it from another arm. Every
 * fragile object is written this way, with the excuse in one arm and the
 * answer in the other, and the callers who want the answer fill nothing.</p>
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
     * Ctor.
     * @param provided What the types certainly have
     * @param filled What the call put into the voids, by the locator of the
     *  void
     * @param voids The locator of every void, from {@link Hollows}
     */
    Branched(
        final Provided provided,
        final Map<String, String> filled,
        final Collection<String> voids
    ) {
        this.owned = provided;
        this.binds = filled;
        this.hollows = voids;
    }

    /**
     * The one thing every formation this call reaches hands back.
     * @return The locator, empty when no formation hands back what it was
     *  given or they share nothing
     */
    String names() {
        final Collection<String> handed = new LinkedHashSet<>(0);
        for (final Map.Entry<String, String> bind : this.binds.entrySet()) {
            final int dot = bind.getKey().lastIndexOf('.');
            if (dot > 0
                && this.hands(bind.getKey().substring(0, dot), bind)
                && this.stands(bind.getValue())) {
                handed.add(bind.getValue());
            }
        }
        return new Joined(handed, this.owned).names();
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
