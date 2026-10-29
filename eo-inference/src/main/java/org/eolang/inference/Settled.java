/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The pairs, asked for again until asking brings nothing.
 *
 * <p>Answering {@code a.b.c} needs {@code a.b} answered first. A pass puts
 * the two in that order itself, since {@link Dispatched#answers(Map, Map)}
 * reads a chain to its end rather than one hop at a time, and the passes run
 * until one of them adds nothing all the same: a void filled by a later pass
 * opens dispatches no earlier one could answer. Pairs are only ever added, of
 * which there are finitely many, so it settles.</p>
 *
 * <p>There are three things to ask, and each is asked only when the one before
 * it has nothing left to say. A dispatch is answered from what the tables hold
 * and costs a walk of them; what a void holds is answered from the whole of
 * the program's call sites and costs a table built to be read once, so it is
 * worth asking when a pass would otherwise be the last. Every void it names
 * opens the dispatches rooted at that void, and the passes go round again.</p>
 *
 * <p>The third is what to write down about a dispatch neither of them ever
 * answered. A question asked of a void is rewritten into a question asked of
 * what fills it, and until the filling settles the rewrite has nowhere to go;
 * a pass that wrote the unrewritten name down then would freeze a site the
 * next pass could have answered (#8351). So the name the tables give is asked
 * for last, once the other two have stopped learning, and a site given up on
 * that way is one no filling was ever going to settle.</p>
 *
 * <p>Before that, where neither of the first two has anything to say, the arms
 * every read off a choice is a copy of are asked for, since a call on such a
 * read fills the voids of its arms and a filling nobody hears of answers
 * nothing (#8993). They cost what a pass costs, so they are asked for only
 * here, and every pass after that is told them. Where they add an arm, the
 * passes go round again before anything is given up on.</p>
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
     *
     * @param dispatched What every dispatch turns out to be
     * @param promoted What the voids the program fills one way turn out to be
     */
    Settled(final Dispatched dispatched, final Promoted promoted) {
        this.made = dispatched;
        this.more = promoted;
    }

    /**
     * The pairs, with everything that follows from them added.
     *
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The pairs and the ones worked out from them
     */
    Map<String, String> from(final Map<String, String> pairs) {
        final Map<String, String> found = new LinkedHashMap<>(pairs);
        Map<String, Collection<String>> copied = Collections.emptyMap();
        boolean learning = true;
        while (learning) {
            Map<String, String> answers = this.answers(found, copied);
            if (answers.isEmpty()) {
                final Map<String, Collection<String>> wider = this.made.copies(found, copied);
                if (wider.equals(copied)) {
                    answers = this.made.guesses(found);
                    learning = !answers.isEmpty();
                }
                copied = wider;
            }
            found.putAll(answers);
        }
        return found;
    }

    private Map<String, String> answers(
        final Map<String, String> pairs, final Map<String, Collection<String>> copied
    ) {
        Map<String, String> found = this.made.answers(pairs, copied);
        if (found.isEmpty()) {
            found = this.more.from(pairs, copied);
        }
        return found;
    }
}
