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
 * through the voids still empty at that point of the chain of copies rather
 * than through all the ones the formation declares: {@code pair 1 > half} fills
 * the first void and {@code half 2 > full} the second, which is the walk
 * {@code PhDefault.vacancy()} makes at run time.</p>
 *
 * <p>What fills the void is written down as the argument itself and not as
 * what the argument turns out to be. The two differ exactly when the argument
 * is an application of its own: {@code inc (dec u)} fills the {@code x} of the
 * {@code inc} with an object that has filled the {@code x} of a {@code dec},
 * and naming {@code Φ.dec} here would throw that half away. The argument has a
 * row of its own, which says both.</p>
 *
 * <p>The receiver of a dispatch fills a void too, when the formation it
 * dispatches into declares one for it. That void is named {@code ρ} and is
 * not counted among the places, since it is answered by whoever dispatches
 * and not by an argument written to the right of the name. A name written by
 * itself dispatches as well, off the object it is written inside, and
 * {@link Taken} answers for both.</p>
 *
 * @since 0.69.0
 */
final class Bound {

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

    /**
     * The arguments of every application bound by name, from {@link Given}.
     */
    private final Map<String, Map<String, String>> named;

    /**
     * What every dispatch takes its attribute from, from {@link Taken}.
     */
    private final Map<String, String> receivers;

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     * @param arguments The arguments of every application, from {@link Given}
     * @param bindings The arguments of every application bound by name
     * @param taken What every dispatch takes its attribute from
     * @param links The pairs, each name against the one it is a copy of
     * @param provided What the types certainly have
     */
    Bound(
        final Map<String, List<String>> arguments,
        final Map<String, Map<String, String>> bindings,
        final Map<String, String> taken,
        final Map<String, String> links,
        final Provided provided
    ) {
        this.args = arguments;
        this.named = bindings;
        this.receivers = taken;
        this.pairs = links;
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
            final Map<String, String> filled = this.filled(application, this.taken(application));
            if (!filled.isEmpty()) {
                found.put(application, filled);
            }
        }
        for (final Map.Entry<String, String> dispatch : this.receivers.entrySet()) {
            final String hollow = this.owned.receiver(this.base(dispatch.getKey()));
            if (!hollow.isEmpty()) {
                found.computeIfAbsent(dispatch.getKey(), key -> new LinkedHashMap<>(1))
                    .put(hollow, dispatch.getValue());
            }
        }
        return found;
    }

    private Map<String, String> filled(final String application, final Collection<String> before) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        final List<String> given = this.args.getOrDefault(application, Collections.emptyList());
        for (int place = 0; place < given.size(); place += 1) {
            final String hollow = this.owned.vacant(this.base(application), before, place);
            if (!hollow.isEmpty() && !given.get(place).isEmpty()) {
                found.put(hollow, given.get(place));
            }
        }
        for (final Map.Entry<String, String> bind
            : this.named.getOrDefault(application, Collections.emptyMap()).entrySet()) {
            final String hollow = this.owned.named(this.base(application), bind.getKey());
            if (!hollow.isEmpty()) {
                found.put(hollow, bind.getValue());
            }
        }
        return found;
    }

    private Collection<String> taken(final String application) {
        final List<String> chain = new ArrayList<>(0);
        final Collection<String> seen = new HashSet<>(0);
        String walked = application;
        while (this.pairs.containsKey(walked) && seen.add(walked)) {
            walked = this.pairs.get(walked);
            chain.add(walked);
        }
        Collections.reverse(chain);
        final Collection<String> found = new HashSet<>(0);
        for (final String step : chain) {
            found.addAll(this.filled(step, found).keySet());
        }
        return found;
    }

    private String base(final String name) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = name;
        while (this.pairs.containsKey(walked) && seen.add(walked)) {
            walked = this.pairs.get(walked);
        }
        return walked;
    }
}
