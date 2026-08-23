/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * What every application in a copy chain has already filled by the time it
 * applies.
 *
 * <p>An application does not always copy the formation itself: {@code half 2
 * > full} copies {@code half}, which is itself a copy of {@code pair}. The
 * voids {@code half} filled are gone by the time {@code full} applies, walked
 * past the way {@code PhDefault.vacancy()} walks past an attribute that
 * already holds something. What an application fills on its own, then, has to
 * be asked with the voids the rest of its chain of copies already took.</p>
 *
 * @since 0.70.0
 */
final class Consumed {

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

    /**
     * Every name against the one it is a direct copy of.
     */
    private final Map<String, String> chain;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     * @param arguments The arguments of every application, from {@link Given}
     * @param copies Every name against the one it is a direct copy of
     * @param provided What the types certainly have
     */
    Consumed(
        final Map<String, List<String>> arguments,
        final Map<String, String> copies,
        final Provided provided
    ) {
        this.args = arguments;
        this.chain = copies;
        this.owned = provided;
    }

    /**
     * What this application fills on its own, once the voids the rest of its
     * chain of copies already took are set aside.
     * @param application The locator of the application
     * @return The objects the voids hold, by the locator of the void
     */
    Map<String, String> filled(final String application) {
        Map<String, String> before = new LinkedHashMap<>(0);
        Map<String, String> mine = new LinkedHashMap<>(0);
        for (final String step : this.order(application)) {
            mine = this.stepped(step, before);
            before = new LinkedHashMap<>(before);
            before.putAll(mine);
        }
        return mine;
    }

    /**
     * What one step of a chain fills on its own.
     * @param step The locator of the application at this step
     * @param before The voids the earlier steps of the chain already took
     * @return The objects the voids hold, by the locator of the void
     */
    private Map<String, String> stepped(final String step, final Map<String, String> before) {
        final Map<String, String> mine = new LinkedHashMap<>(0);
        final String root = this.root(step);
        final List<String> given = this.args.get(step);
        for (int place = 0; place < given.size(); place += 1) {
            final String arg = given.get(place);
            if (!arg.isEmpty()) {
                final String hollow = this.owned.slot(root, place, before.keySet());
                if (!hollow.isEmpty()) {
                    mine.put(hollow, arg);
                }
            }
        }
        return mine;
    }

    /**
     * The chain of a node's own copies, from the furthest to the nearest.
     * @param node The locator to start from
     * @return The applications on the way, the node itself last
     */
    private List<String> order(final String node) {
        final List<String> steps = new ArrayList<>(0);
        final Collection<String> seen = new HashSet<>(0);
        String walked = node;
        while (this.args.containsKey(walked) && seen.add(walked)) {
            steps.add(walked);
            walked = this.chain.getOrDefault(walked, "");
        }
        Collections.reverse(steps);
        return steps;
    }

    /**
     * The formation at the end of a node's chain of copies.
     * @param type The locator to start from
     * @return The locator of the formation, or the type itself when it copies
     *  nothing
     */
    private String root(final String type) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = type;
        while (this.chain.containsKey(walked) && seen.add(walked)) {
            walked = this.chain.get(walked);
        }
        return walked;
    }
}
