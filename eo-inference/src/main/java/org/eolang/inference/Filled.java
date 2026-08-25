/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * What a name taken from a void turns out to be, where the void is filled.
 *
 * <p>{@link Provided} answers a question about a void with the void itself:
 * the {@code next} of whatever fills {@code x} is {@code Φ.inc.x.next}, which
 * is true of every caller and concrete for none. A receiver reached through an
 * application is a caller, though, and the argument it put in that place says
 * what the void holds. So the void is taken out of the answer, what fills it is
 * put in, and the rest of the names are asked again one by one — the {@code
 * next} of a {@code t} rather than the {@code next} of whatever fills
 * {@code x}.</p>
 *
 * <p>The fillings are gathered along the whole chain a receiver resolves
 * through, since the application that fills a void and the dispatch that asks
 * about it are rarely the same object: {@code inc t > held} fills the void and
 * {@code held.next} asks. A void filled nearer the question wins over one
 * filled further away, and a void nobody fills keeps the answer as it was.</p>
 *
 * @since 0.69.0
 */
final class Filled {

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * The provides table, by the name a type goes by.
     */
    private final Provided owned;

    /**
     * Ctor.
     * @param arguments The arguments of every application, from {@link Given}
     * @param links The pairs, each name against the one it is a copy of
     * @param provided The provides table, by the name a type goes by
     */
    Filled(
        final Map<String, List<String>> arguments,
        final Map<String, String> links,
        final Provided provided
    ) {
        this.args = arguments;
        this.pairs = links;
        this.owned = provided;
    }

    /**
     * What this answer turns out to be for this receiver.
     * @param answer The type of the attribute, as the table gave it
     * @param bearer The locator of the receiver the question was asked of
     * @return The type the answer stands for here, or the answer itself when
     *  no caller says what the void holds
     */
    String instead(final String answer, final String bearer) {
        final Map<String, String> fillings = this.fillings(bearer);
        final String found;
        if (fillings.containsKey(answer)) {
            found = fillings.get(answer);
        } else {
            String longest = "";
            for (final String hollow : fillings.keySet()) {
                if (answer.startsWith(hollow.concat("."))
                    && hollow.length() > longest.length()) {
                    longest = hollow;
                }
            }
            if (longest.isEmpty()) {
                found = answer;
            } else {
                found = this.asked(
                    fillings.get(longest), answer.substring(longest.length() + 1), answer
                );
            }
        }
        return found;
    }

    /**
     * @todo #7446:35min Let this pass skip the voids already taken too.
     *  The walk below asks {@link Provided#slot(String, int, Collection)}
     *  with nothing taken, so an application that copies a copy lands its
     *  argument on a void an earlier application in the same chain has
     *  already filled, which is the bug #7446 is about. {@link Bound} walks
     *  the chain and keeps what each step took; do the same here, and add a
     *  test that a chain applying twice fills the second void.
     */
    private Map<String, String> fillings(final String bearer) {
        final Map<String, String> found = new HashMap<>(0);
        final Collection<String> seen = new HashSet<>(0);
        String walked = bearer;
        while (this.pairs.containsKey(walked) && seen.add(walked)) {
            final String copied = this.pairs.get(walked);
            if (this.args.containsKey(walked)) {
                final List<String> given = this.args.get(walked);
                for (int place = 0; place < given.size(); place += 1) {
                    final String hollow = this.owned.slot(
                        this.end(copied), place, Collections.emptySet()
                    );
                    if (!hollow.isEmpty() && !given.get(place).isEmpty()) {
                        found.putIfAbsent(hollow, this.end(given.get(place)));
                    }
                }
            }
            walked = copied;
        }
        return found;
    }

    private String asked(final String start, final String names, final String back) {
        String walked = start;
        int from = 0;
        while (from < names.length() && !walked.isEmpty()) {
            int next = names.indexOf('.', from);
            if (next < 0) {
                next = names.length();
            }
            walked = this.owned.attribute(walked, names.substring(from, next));
            from = next + 1;
        }
        if (walked.isEmpty()) {
            walked = back;
        }
        return walked;
    }

    private String end(final String locator) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = locator;
        while (this.pairs.containsKey(walked) && seen.add(walked)) {
            walked = this.pairs.get(walked);
        }
        return walked;
    }
}
