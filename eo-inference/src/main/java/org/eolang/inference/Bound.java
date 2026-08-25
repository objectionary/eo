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
 * What every application puts into the voids of what it copies.
 *
 * <p>An application is the one place in a program where a void stops being a
 * question: {@code inc t} says that the {@code x} of that {@code inc} holds a
 * {@code t}. The place of an argument says which void it lands in, counted
 * through the voids in the order the formation declares them, which is the
 * order {@link Ungrouped} keeps, past the voids the rest of the chain of
 * copies already took: {@code half 2 > full} copies {@code half}, which is a
 * copy of {@code pair}, and the void {@code half} filled is gone by the time
 * {@code full} applies, the way {@code PhDefault.vacancy()} walks past an
 * attribute that already holds something.</p>
 *
 * <p>What fills the void is written down as the argument itself and not as
 * what the argument turns out to be. The two differ exactly when the argument
 * is an application of its own: {@code inc (dec u)} fills the {@code x} of the
 * {@code inc} with an object that has filled the {@code x} of a {@code dec},
 * and naming {@code Φ.dec} here would throw that half away. The argument has a
 * row of its own, which says both.</p>
 *
 * @since 0.69.0
 */
final class Bound {

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
    Bound(
        final Map<String, List<String>> arguments,
        final Map<String, String> copies,
        final Provided provided
    ) {
        this.args = arguments;
        this.chain = copies;
        this.owned = provided;
    }

    /**
     * What every application fills, by the locator of the application.
     * @return The objects the voids hold, by the locator of the void, in the
     *  order the voids were declared, without the applications that fill
     *  nothing we can name
     */
    Map<String, Map<String, String>> all() {
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        for (final String application : this.args.keySet()) {
            final Map<String, String> filled = this.filled(application);
            if (!filled.isEmpty()) {
                found.put(application, filled);
            }
        }
        return found;
    }

    private Map<String, String> filled(final String application) {
        final Collection<String> taken = new HashSet<>(0);
        Map<String, String> mine = new LinkedHashMap<>(0);
        for (final String step : this.order(application)) {
            mine = this.stepped(step, taken);
            taken.addAll(mine.keySet());
        }
        return mine;
    }

    private Map<String, String> stepped(final String step, final Collection<String> taken) {
        final Map<String, String> mine = new LinkedHashMap<>(0);
        final String copied = this.root(step);
        final List<String> given = this.args.get(step);
        for (int place = 0; place < given.size(); place += 1) {
            final String hollow = this.owned.slot(copied, place, taken);
            if (!hollow.isEmpty() && !given.get(place).isEmpty()) {
                mine.put(hollow, given.get(place));
            }
        }
        return mine;
    }

    private String root(final String type) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = type;
        while (this.chain.containsKey(walked) && seen.add(walked)) {
            walked = this.chain.get(walked);
        }
        return walked;
    }

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
}
