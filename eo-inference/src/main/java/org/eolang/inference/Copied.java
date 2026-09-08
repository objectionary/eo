/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * What every application fills of what it is a copy of, and of nothing else.
 *
 * <p>{@link Bound} reads the pair of an application to know which voids there
 * are to fill. That is right for as long as the pair says what the application
 * copies, and a call answered by a formation a void hands back has a pair that
 * says something else: the {@code ^.if} of an {@code and} is a copy of the void
 * {@code Φ.bool.if}, which declares no place for an argument, but its pair
 * reads {@code Φ.bool} once {@link Branched} has had its say, and a
 * {@code Φ.bool} does declare one.</p>
 *
 * <p>So the {@code FF-.eq x} of that {@code and} lands in the {@code if} of
 * {@code Φ.bool} and stands there for every reader of that void in the
 * program, which is a whole library told that a boolean chooses with a
 * comparison (#8508). {@link Lent} says which calls those are, and what they
 * put into the voids of their answer is dropped. What they put into the
 * formations the void holds is kept, since that is where their arguments really
 * went.</p>
 *
 * @since 0.71.0
 */
final class Copied {

    /**
     * What every application fills, from {@link Bound}.
     */
    private final Map<String, Map<String, String>> fills;

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * The calls that land on a void, from {@link Lent}.
     */
    private final Collection<String> lent;

    /**
     * Ctor.
     *
     * @param bound What every application fills, from {@link Bound}
     * @param links The pairs, each name against the one it is a copy of
     * @param sites The calls that land on a void, from {@link Lent}
     */
    Copied(
        final Map<String, Map<String, String>> bound,
        final Map<String, String> links,
        final Collection<String> sites
    ) {
        this.fills = bound;
        this.pairs = links;
        this.lent = sites;
    }

    /**
     * What every application fills, by the locator of the application.
     *
     * @return The objects the voids hold, by the locator of the void, without
     *  the voids of an answer the call was handed rather than copied
     */
    Map<String, Map<String, String>> all() {
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, Map<String, String>> call : this.fills.entrySet()) {
            found.put(call.getKey(), this.kept(call.getKey(), call.getValue()));
        }
        return found;
    }

    private Map<String, String> kept(final String call, final Map<String, String> filled) {
        final Map<String, String> found;
        if (this.lent.contains(call)) {
            final String answer = new Ends(this.pairs).name(call).concat(".");
            found = new LinkedHashMap<>(filled);
            found.keySet().removeIf(hollow -> hollow.startsWith(answer));
        } else {
            found = filled;
        }
        return found;
    }
}
