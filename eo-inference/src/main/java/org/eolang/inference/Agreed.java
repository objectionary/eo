/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * The answers that came out one way.
 *
 * <p>The loop asks what a computed object turns out to be, and asks it once
 * for every copy of the formation the dispatch stands in. One answer is a
 * fact. Two are a locator that is not one object, and the honest thing to do
 * with it is nothing at all — it goes by its own name, the tables say nothing
 * about it, and every check about it stays undecided.</p>
 *
 * @since 0.68.0
 */
final class Agreed {

    /**
     * Every answer given, by the locator it was given about.
     */
    private final Map<String, Collection<String>> answers;

    /**
     * Ctor.
     * @param given Every answer given, by the locator it was given about
     */
    Agreed(final Map<String, Collection<String>> given) {
        this.answers = given;
    }

    /**
     * The name every computed object goes by.
     * @return The locators that came out one way, against that one answer
     */
    Map<String, String> names() {
        final Map<String, String> found = new HashMap<>(this.answers.size());
        for (final Map.Entry<String, Collection<String>> answer : this.answers.entrySet()) {
            if (answer.getValue().size() == 1) {
                found.put(answer.getKey(), answer.getValue().iterator().next());
            }
        }
        return found;
    }
}
