/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import org.xembly.Directives;

/**
 * What the arguments of one application land in, once the void it copies is
 * known to hold a formation.
 *
 * <p>The places are counted through the voids of that formation exactly as
 * {@link Bound} counts them through the voids of a formation an application
 * names outright, and what comes back is written the same way, as a
 * {@code bind} in the row of the application.</p>
 *
 * <p>Every formation the void is seen to hold is asked in turn, since neither
 * of two callers is more the answer than the other.</p>
 *
 * <p>A filling is a locator written at a call site and rarely the formation
 * itself: {@code malloc.for 0 x} puts an argument of its own into the void, and
 * that argument is a copy of the {@code x} beside it. The voids are declared by
 * the end of that chain of copies, so the filling is asked for by the name it
 * goes by rather than by the name it is written under, and only a formation
 * written out at the call site answers to both (#8389).</p>
 *
 * @since 0.70.0
 */
final class Passed {

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * The locators of what the void is seen to hold.
     */
    private final Collection<String> fillers;

    /**
     * The locators of the arguments, in the order they are written.
     */
    private final List<String> args;

    /**
     * Ctor.
     * @param provided What the types certainly have
     * @param aliases The name every type goes by, from {@link Ends}
     * @param holds The locators of what the void is seen to hold
     * @param arguments The locators of the arguments, in the order they are
     *  written
     */
    Passed(
        final Provided provided,
        final Map<String, String> aliases,
        final Collection<String> holds,
        final List<String> arguments
    ) {
        this.owned = provided;
        this.names = aliases;
        this.fillers = holds;
        this.args = arguments;
    }

    /**
     * What the arguments fill, as the contents of the application's row.
     * @return The directives, empty when the arguments land nowhere
     */
    Directives directives() {
        final Directives dirs = new Directives();
        for (final String filler : this.fillers) {
            final String held = this.names.getOrDefault(filler, filler);
            for (int place = 0; place < this.args.size(); place += 1) {
                final String slot = this.owned.vacant(held, Collections.emptyList(), place);
                if (!slot.isEmpty() && !this.args.get(place).isEmpty()) {
                    dirs.add("bind")
                        .attr("void", slot)
                        .add("ref")
                        .attr("loc", this.args.get(place))
                        .up()
                        .up();
                }
            }
        }
        return dirs;
    }
}
