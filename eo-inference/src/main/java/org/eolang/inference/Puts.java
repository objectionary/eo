/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
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
     * What this void holds.
     *
     * @param hollow The locator of the void
     * @return The objects any call put into it, empty when nobody fills it
     */
    Collection<String> holders(final String hollow) {
        return this.holds.getOrDefault(hollow, Collections.emptySet());
    }
}
