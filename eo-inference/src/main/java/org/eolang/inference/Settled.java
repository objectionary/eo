/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The pairs, asked for again until asking brings nothing.
 *
 * <p>Answering {@code a.b.c} needs {@code a.b} answered first, and one pass
 * cannot put them in that order, so the passes run until one of them adds
 * nothing. Pairs are only ever added, of which there are finitely many, so it
 * settles.</p>
 *
 * @since 0.69.0
 */
final class Settled {

    /**
     * What every dispatch turns out to be.
     */
    private final Dispatched made;

    /**
     * Ctor.
     * @param dispatched What every dispatch turns out to be
     */
    Settled(final Dispatched dispatched) {
        this.made = dispatched;
    }

    /**
     * The pairs, with everything that follows from them added.
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The pairs and the ones worked out from them
     */
    Map<String, String> from(final Map<String, String> pairs) {
        final Map<String, String> found = new LinkedHashMap<>(pairs);
        Map<String, String> answers = this.made.answers(found);
        while (!answers.isEmpty()) {
            found.putAll(answers);
            answers = this.made.answers(found);
        }
        return found;
    }
}
