/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Map;
import org.xembly.Directives;

/**
 * What whatever fills a void will have to offer.
 *
 * <p>A void holds nothing until a caller puts something in it, so nothing can
 * be said about what it <em>is</em>. Plenty can be said about what it must
 * have, though, and the program says it: every name taken from the void is a
 * name its value will have to answer to. {@code number} keeps one void and
 * hands everything to it, and the program asks that void for {@code and},
 * {@code as-bytes}, {@code eq} and {@code size} — which is to say that only a
 * {@code bytes} will do, without a word of it ever being written down.</p>
 *
 * <p>A name taken from the answer is a demand in its turn, of the object one
 * step deeper: {@code x.next.foo} says that {@code x} must have a {@code next}
 * and that the {@code next} must have a {@code foo}. Every one of those objects
 * is a name rooted at the void, so all of them are gathered here, each demand
 * saying which one it is made of.</p>
 *
 * <p>They are written side by side rather than one inside the other, which
 * would have read better, because the chain is not always there to nest them
 * along: {@code while} hands its answers through several objects to the
 * {@code if} of the {@code as-bool} of what fills {@code Φ.bool.if}, and
 * nobody ever asked for that {@code as-bool} — the delegation went through it.
 * Nesting drops every demand whose chain was walked that way rather than
 * asked for, 77 of the 2,303 in eo-runtime, and a fact that cannot be written
 * where it belongs is still a fact.</p>
 *
 * <p>A name is not the only thing the program asks of a void. It also applies
 * one, and that demand is {@link Applies}, written beside these on the same
 * row and checked the other way round.</p>
 *
 * @since 0.69.0
 */
final class Demands {

    /**
     * What is asked of every object, from {@link Asked}.
     */
    private final Map<String, Map<String, String>> asked;

    /**
     * The voids these demands are made of.
     */
    private final Rooted rooted;

    /**
     * Ctor.
     * @param all What is asked of every object, from {@link Asked}
     * @param objects The voids these demands are made of: the void itself,
     *  and every void it is handed into
     */
    Demands(final Map<String, Map<String, String>> all, final Rooted objects) {
        this.asked = all;
        this.rooted = objects;
    }

    /**
     * These demands, to be put inside the row of the void.
     * @return The directives, empty when nothing is ever asked of it
     */
    Directives directives() {
        final Directives dirs = new Directives();
        for (final Map.Entry<String, Map<String, String>> bearer : this.asked.entrySet()) {
            if (this.rooted.covers(bearer.getKey())) {
                for (final Map.Entry<String, String> demand : bearer.getValue().entrySet()) {
                    dirs.add("demand")
                        .attr("of", bearer.getKey())
                        .attr("name", demand.getKey())
                        .attr("type", demand.getValue())
                        .up();
                }
            }
        }
        return dirs;
    }

    /**
     * Whether anything is ever asked of this void, or of a name rooted at it.
     * @return True when at least one name is
     */
    boolean any() {
        boolean found = false;
        for (final String bearer : this.asked.keySet()) {
            if (this.rooted.covers(bearer)) {
                found = true;
                break;
            }
        }
        return found;
    }
}
