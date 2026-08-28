/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
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
 * <p>Every formation the void is seen to hold is asked in turn. Two callers
 * may put formations of different shapes into one void, and neither of them
 * is more the answer than the other, so an argument lands in both.</p>
 *
 * @since 0.70.0
 */
final class Passed {

    /**
     * What the types certainly have.
     */
    private final Provided owned;

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
     * @param holds The locators of what the void is seen to hold
     * @param arguments The locators of the arguments, in the order they are
     *  written
     */
    Passed(final Provided provided, final Collection<String> holds, final List<String> arguments) {
        this.owned = provided;
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
            for (int place = 0; place < this.args.size(); place += 1) {
                final String slot = this.owned.vacant(filler, Collections.emptyList(), place);
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
