/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * The arms every read off a choice is a copy of.
 *
 * <p>A read off a choice is the attribute of that name in every arm:
 * {@code n.wide.div}, where {@code wide} comes back as a {@code big} or as
 * whatever its {@code fail} holds, is the {@code div} of that {@code big}, and
 * a call written on it fills the voids of that {@code div} (#8883).</p>
 *
 * <p>Not every arm of a call is one. A picker comes back with one of the
 * objects it was handed, so the arms of {@code flag.if a b} are {@code a} and
 * {@code b}, and they are what the call gives back and not what it copies:
 * its arguments go into the voids of the {@code if}, and never into the
 * voids of {@code a}. So only an arm of the name the call reads is kept.</p>
 *
 * @since 0.76.0
 * @todo #8883:90min Credit the arms of a read off a choice while the passes run.
 *  The choices are asked for once, by {@link Woven}, after {@link Settled} has
 *  stopped, so only the rows and the census of the provides table hear of
 *  these fillings, while the {@link Holders} that {@link Dispatched} builds on
 *  every pass never does, and {@link Branched} still takes such a void for one
 *  nobody fills. Asking for the choices on every pass costs a second
 *  {@link Bound}, which is most of what a pass costs, so find a way that does
 *  not before rule 5 of #8977 drops an arm for being empty.
 */
final class Copied {

    /**
     * Every dispatch and read of the program.
     */
    private final Collection<Site> sites;

    /**
     * What every call on a void may come back with, from {@link Dispatched}.
     */
    private final Map<String, Collection<String>> arms;

    /**
     * Ctor.
     *
     * @param dispatches Every dispatch and read of the program
     * @param chosen What every call on a void may come back with
     */
    Copied(final Collection<Site> dispatches, final Map<String, Collection<String>> chosen) {
        this.sites = dispatches;
        this.arms = chosen;
    }

    /**
     * The arms, by the locator of the read.
     *
     * @return The arms each read is a copy of, without the reads that are a
     *  copy of none
     */
    Map<String, Collection<String>> all() {
        final Map<String, Collection<String>> found = new HashMap<>(0);
        for (final Site dispatch : this.sites) {
            final String suffix = String.format(".%s", dispatch.name());
            for (final String arm
                : this.arms.getOrDefault(dispatch.made(), Collections.emptyList())) {
                if (arm.endsWith(suffix)) {
                    found.computeIfAbsent(dispatch.made(), key -> new LinkedHashSet<>(1))
                        .add(arm);
                }
            }
        }
        return found;
    }
}
