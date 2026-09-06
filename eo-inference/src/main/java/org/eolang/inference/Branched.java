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
 * is a question for whoever walks a delegation and not for this.</p>
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
     * Ctor.
     * @param provided What the types certainly have
     * @param filled What the call put into the voids, by the locator of the
     *  void
     */
    Branched(final Provided provided, final Map<String, String> filled) {
        this.owned = provided;
        this.binds = filled;
    }

    /**
     * The one thing every formation this call reaches hands back.
     * @return The locator, empty when no formation hands back what it was
     *  given or they share nothing
     */
    String names() {
        final Collection<String> handed = new LinkedHashSet<>(0);
        for (final String hollow : this.binds.keySet()) {
            final int dot = hollow.lastIndexOf('.');
            if (dot > 0) {
                final String body = this.owned.behind(hollow.substring(0, dot));
                if (this.binds.containsKey(body)) {
                    handed.add(this.binds.get(body));
                }
            }
        }
        return new Joined(handed, this.owned).names();
    }
}
