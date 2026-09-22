/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * What the calls of a program put into its voids, from both sides.
 *
 * <p>The same facts are wanted two ways. What a call is a copy of is answered
 * by what that one call filled, and what a void holds is answered by what every
 * call filled. So both sides are kept here, the second worked out once by
 * {@link Holders} rather than looked for again at every question.</p>
 *
 * @since 0.71.0
 */
final class Puts {

    /**
     * What every application fills, from {@link Bound}.
     */
    private final Map<String, Map<String, String>> fills;

    /**
     * What every void holds, from {@link Holders}.
     */
    private final Map<String, Collection<String>> holds;

    /**
     * Ctor.
     *
     * @param bound What every application fills, from {@link Bound}
     * @param holders What every void holds, from {@link Holders}
     */
    Puts(
        final Map<String, Map<String, String>> bound,
        final Map<String, Collection<String>> holders
    ) {
        this.fills = bound;
        this.holds = holders;
    }

    /**
     * What this call fills.
     *
     * @param call The locator of the application
     * @return The objects the voids hold, by the locator of the void, empty
     *  when this call fills nothing
     */
    Map<String, String> at(final String call) {
        return this.fills.getOrDefault(call, Collections.emptyMap());
    }

    /**
     * Whether any call of the program puts anything into this void.
     *
     * <p>A void nobody fills terminates the moment it is read, and a void the
     * callers fill holds whatever they put there. The two read alike in a
     * locator and are worlds apart in what an arm rooted at one is worth.</p>
     *
     * @param hollow The locator of the void
     * @return True when at least one call of the program fills it
     */
    boolean fills(final String hollow) {
        return this.holds.containsKey(hollow);
    }

    /**
     * Which of these fillings went into a formation this void holds.
     *
     * <p>A call fills the voids of whatever it copies, and only the arms of a
     * formation the void holds say what a call on the void hands back. An
     * argument is relayed to every formation the void might hold, so the rest
     * of what the same call filled belongs to somebody else and would read as
     * an arm it never was.</p>
     *
     * @param arms What a call filled, by the locator of the void it filled
     * @param hollow The locator of the void
     * @return The fillings, by the locator of the void, empty when the call
     *  filled nothing of what this void holds
     */
    Map<String, String> armed(final Map<String, String> arms, final String hollow) {
        final Collection<String> holders =
            this.holds.getOrDefault(hollow, Collections.emptySet());
        final Map<String, String> found = new HashMap<>(0);
        for (final Map.Entry<String, String> arm : arms.entrySet()) {
            final int dot = arm.getKey().lastIndexOf('.');
            if (dot > 0 && holders.contains(arm.getKey().substring(0, dot))) {
                found.put(arm.getKey(), arm.getValue());
            }
        }
        return found;
    }
}
