/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * The void an argument of an application lands in, counted among the voids that
 * are still empty at that point of the copy chain.
 *
 * <p>An argument goes into a void by its place, and the place is counted through
 * the voids that are still empty, not through all the voids the formation
 * declares. The two differ once an object is built by more than one application:
 * {@code pair 1 > half} fills the first void of {@code pair}, and
 * {@code half 2 > full} fills the second, because the first is not empty any
 * more by the time {@code full} is copied. This is the same walk {@code
 * PhDefault.put(int, Phi)} makes through {@code vacancy()} at run time, so the
 * table and the run agree on which void an argument holds.</p>
 *
 * @since 0.69.0
 */
final class Vacancy {

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

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
     * @param links The pairs, each name against the one it is a copy of
     * @param provided What the types certainly have
     */
    Vacancy(
        final Map<String, List<String>> arguments,
        final Map<String, String> links,
        final Provided provided
    ) {
        this.args = arguments;
        this.pairs = links;
        this.owned = provided;
    }

    /**
     * The void this application fills in the given place.
     * @param application The locator of the application
     * @param place The place of the argument, among the voids still empty when
     *  this application is copied
     * @return The locator of the void, or an empty string when there is no such
     *  empty void
     */
    String at(final String application, final int place) {
        return this.owned.vacant(
            this.base(application),
            this.filled(this.pairs.get(application), new HashSet<>(0)),
            place
        );
    }

    private String base(final String name) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = name;
        while (this.pairs.containsKey(walked) && seen.add(walked)) {
            walked = this.pairs.get(walked);
        }
        return walked;
    }

    private Set<Integer> filled(final String object, final Collection<String> seen) {
        final Set<Integer> found = new HashSet<>(0);
        if (object != null && this.args.containsKey(object) && seen.add(object)) {
            final Set<Integer> before = this.filled(this.pairs.get(object), seen);
            found.addAll(before);
            final String base = this.base(object);
            final List<String> given = this.args.get(object);
            for (int place = 0; place < given.size(); place += 1) {
                final int index;
                if (given.get(place).isEmpty()) {
                    index = -1;
                } else {
                    index = this.owned.index(base, before, place);
                }
                if (index >= 0) {
                    found.add(index);
                }
            }
        }
        return found;
    }
}
