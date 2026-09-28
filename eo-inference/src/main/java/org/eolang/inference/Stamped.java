/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

/**
 * The object a name written after a dot hands its receiver to.
 *
 * <p>{@code a.b} takes {@code b} off {@code a}, and the runtime stamps the
 * {@code a} on whatever {@code b} is a copy of, as long as that copy has no
 * receiver of its own yet. So the chain of copies is walked from {@code b},
 * and the walk stops at the first object that settles it. A formation that
 * declares {@code ^} settles it one way, since that is where the receiver
 * goes: {@code board.raised}, where the board says {@code flag > raised},
 * fills the {@code ^} of that {@code flag}. A copy that was itself taken off
 * something settles it the other way, since it got its receiver where it was
 * written: {@code os.is-windows} is a copy of {@code name.contains}, and the
 * {@code os} goes nowhere (#8955).</p>
 *
 * <p>A bare name of the formation's own does not settle it, though. The
 * {@code get} of a {@code chunk} is a copy of its {@code read}, which it takes
 * off whatever it is read from, so {@code c.get} fills the {@code ^} of that
 * {@code read} with the {@code c}.</p>
 *
 * <p>A formation declaring {@code ^} is asked about first, because the parser
 * writes that {@code ^} where it writes the receiver of a dispatch, and
 * {@link Xmirs} finds the two the same way.</p>
 *
 * @since 0.76.0
 */
final class Stamped {

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * What every dispatch takes its attribute from, from {@link Taken}.
     */
    private final Map<String, String> receivers;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     *
     * @param links The pairs, each name against the one it is a copy of
     * @param taken What every dispatch takes its attribute from
     * @param provided What the types certainly have
     */
    Stamped(
        final Map<String, String> links, final Map<String, String> taken,
        final Provided provided
    ) {
        this.pairs = links;
        this.receivers = taken;
        this.owned = provided;
    }

    /**
     * The object the receiver of this dispatch lands on.
     *
     * @param dispatch The locator of a dispatch written after a dot
     * @return The locator of the object whose receiver it fills, or an empty
     *  string when the copy it reaches has a receiver already
     */
    String names(final String dispatch) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = this.pairs.getOrDefault(dispatch, "");
        while (this.owned.receiver(walked).isEmpty() && !this.dotted(walked)
            && this.pairs.containsKey(walked) && seen.add(walked)) {
            walked = this.pairs.get(walked);
        }
        final String found;
        if (this.owned.receiver(walked).isEmpty() && this.dotted(walked)) {
            found = "";
        } else if (this.owned.receiver(walked).isEmpty()) {
            found = new Ends(this.pairs).name(walked);
        } else {
            found = walked;
        }
        return found;
    }

    private boolean dotted(final String name) {
        return this.receivers.getOrDefault(name, "").equals(name.concat(".ρ"));
    }
}
