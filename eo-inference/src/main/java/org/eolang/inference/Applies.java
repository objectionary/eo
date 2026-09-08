/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import org.xembly.Directives;

/**
 * What whatever fills a void will have to take.
 *
 * <p>{@link Demands} gathers the names a void is asked for, and a name is only
 * half of what the program asks. A void is also applied: {@code while} writes
 * {@code ^.body index}, {@code malloc} writes {@code ^.scope m}, and neither
 * of those is a name taken off anything. Whatever fills such a void will have
 * to keep a void of its own, in that place, that the argument fits (#8158).</p>
 *
 * <p>The two are checked in opposite directions, which is why they are written
 * apart. A demand for a name is met when the filler offers that name, so the
 * filler is asked what it has. A call is met when the filler's own void takes
 * what was passed, so what is asked is the argument, of the filler — the
 * question runs from the call site inwards rather than from the filler out.</p>
 *
 * <p>A call made on a name rooted at the void belongs here as much as one made
 * on the void itself, since the object that name arrives at is handed over by
 * whatever fills the void. {@link Rooted} is the one place that is worked
 * out.</p>
 *
 * @since 0.72.0
 */
final class Applies {

    /**
     * Every call the program makes, from {@link Calls}.
     */
    private final Collection<Call> made;

    /**
     * The voids these calls are made on.
     */
    private final Rooted rooted;

    /**
     * Ctor.
     * @param calls Every call the program makes, from {@link Calls}
     * @param voids The voids these calls are made on: the void itself, and
     *  every void it is handed into
     */
    Applies(final Collection<Call> calls, final Rooted voids) {
        this.made = calls;
        this.rooted = voids;
    }

    /**
     * These calls, to be put inside the row of the void.
     * @return The directives, empty when the void is never applied
     */
    Directives directives() {
        final Directives dirs = new Directives();
        for (final Call call : this.made) {
            if (this.rooted.covers(call.of())) {
                dirs.append(call.directives());
            }
        }
        return dirs;
    }

    /**
     * Whether the void is ever applied, or a name rooted at it is.
     * @return True when at least one call is made on it
     */
    boolean any() {
        boolean found = false;
        for (final Call call : this.made) {
            if (this.rooted.covers(call.of())) {
                found = true;
                break;
            }
        }
        return found;
    }
}
