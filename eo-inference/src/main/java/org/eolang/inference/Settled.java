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
 * <p>There are two things to ask, and the second is asked only when the first
 * has nothing left to say. A dispatch is answered from what the tables hold
 * and costs a walk of them; what a void holds is answered from the whole of
 * the program's call sites and costs a table built to be read once, so it is
 * worth asking when a pass would otherwise be the last. Every void it names
 * opens the dispatches rooted at that void, and the passes go round again.</p>
 *
 * @since 0.69.0
 */
final class Settled {

    /**
     * What every dispatch turns out to be.
     */
    private final Dispatched made;

    /**
     * What the voids the program fills one way turn out to be.
     */
    private final Promoted more;

    /**
     * Ctor.
     * @param dispatched What every dispatch turns out to be
     * @param promoted What the voids the program fills one way turn out to be
     */
    Settled(final Dispatched dispatched, final Promoted promoted) {
        this.made = dispatched;
        this.more = promoted;
    }

    /**
     * The pairs, with everything that follows from them added.
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The pairs and the ones worked out from them
     */
    Map<String, String> from(final Map<String, String> pairs) {
        final Map<String, String> found = new LinkedHashMap<>(pairs);
        Map<String, String> answers = this.answers(found);
        while (!answers.isEmpty()) {
            found.putAll(answers);
            answers = this.answers(found);
        }
        return found;
    }

    private Map<String, String> answers(final Map<String, String> pairs) {
        Map<String, String> found = this.made.answers(pairs);
        if (found.isEmpty()) {
            found = this.more.from(pairs);
        }
        return found;
    }
}
