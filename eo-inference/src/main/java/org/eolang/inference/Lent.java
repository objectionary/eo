/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

/**
 * The calls whose answer is lent to them rather than copied by them.
 *
 * <p>A dispatch is a copy of the attribute it takes, and a pair says so. Not
 * every dispatch, though: one that lands on a void is a copy of that void, and
 * what it turns out to be is whatever the callers put there. Where they all put
 * a formation that hands an argument back, {@link Branched} says the call is
 * one of its own arguments, and the pair is written with that name — the
 * {@code ^.if} of an {@code and} comes out a {@code Φ.bool} because both the
 * {@code Φ.true} and the {@code Φ.false} that {@code Φ.bool.if} may hold answer
 * with one of the two things given to them.</p>
 *
 * <p>The call is still a copy of the void, and of nothing else. One pair cannot
 * say both, so the second fact is asked for here instead, of two halves: the
 * attribute the dispatch takes is looked up again, and a call that lands on a
 * void is a candidate whatever its pair has since been refined to; and its pair
 * is compared with the one thing its own arguments all are, since that is the
 * only name an arm can hand back. Nothing is remembered from pass to pass —
 * both halves are asked of the tables as they stand, and neither of them is
 * what the refinement wrote (#8508).</p>
 *
 * <p>A name read without a dot before it is such a call too. The {@code if} of
 * {@code if > not} is the void of the {@code bool} the line is written in, and
 * the {@code false} and {@code true} beside it go into whatever fills that
 * void, never into the void itself. Nothing stands beside the name to say what
 * it is read off, so {@link Taken} is asked, which is the one answer that
 * covers a name written after a dot and a name written by itself alike
 * (#8469).</p>
 *
 * @since 0.71.0
 */
final class Lent {

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Every dispatch and read of the program.
     */
    private final Collection<Site> all;

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

    /**
     * What every call takes its attribute from, from {@link Taken}.
     */
    private final Map<String, String> receivers;

    /**
     * Ctor.
     *
     * @param provided What the types certainly have
     * @param dispatches Every dispatch and read of the program
     * @param arguments The arguments of every application, from {@link Given}
     * @param taken What every call takes its attribute from, from {@link Taken}
     */
    Lent(
        final Provided provided,
        final Collection<Site> dispatches,
        final Map<String, List<String>> arguments,
        final Map<String, String> taken
    ) {
        this.owned = provided;
        this.all = dispatches;
        this.args = arguments;
        this.receivers = taken;
    }

    /**
     * The calls whose answer is one of their own arguments.
     *
     * @param names The name every locator goes by, from {@link Ends}
     * @return The locator of every call that lands on a void and comes back as
     *  what it was given, empty when this pass looks into no void
     */
    Collection<String> sites(final Map<String, String> names) {
        final Collection<String> found = new HashSet<>(0);
        for (final Site dispatch : this.all) {
            final String made = dispatch.made();
            final String bearer = this.bearer(dispatch);
            final String given = this.joined(made, names);
            if (!given.isEmpty()
                && given.equals(names.getOrDefault(made, made))
                && this.owned.hollow(
                    this.owned.attribute(names.getOrDefault(bearer, bearer), dispatch.name())
                )) {
                found.add(made);
            }
        }
        return found;
    }

    private String bearer(final Site dispatch) {
        String found = dispatch.bearer();
        if (found.isEmpty()) {
            found = this.receivers.getOrDefault(dispatch.made(), "");
        }
        return found;
    }

    private String joined(final String call, final Map<String, String> names) {
        final Collection<String> arms = new LinkedHashSet<>(0);
        for (final String arm : this.args.getOrDefault(call, Collections.emptyList())) {
            if (!arm.isEmpty()) {
                arms.add(names.getOrDefault(arm, arm));
            }
        }
        String found = "";
        if (!arms.isEmpty()) {
            found = new Joined(arms, this.owned).names();
        }
        return found;
    }
}
