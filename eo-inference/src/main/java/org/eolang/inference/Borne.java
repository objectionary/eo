/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;

/**
 * The objects the receiver of a dispatch comes back with.
 *
 * <p>A read takes its arms from the row of the call it is written on, since
 * the {@code plus} of a call that came back with a {@code dial} or a
 * {@code clock} is one of two {@code plus}es. That row is silent when the call
 * settled on an object of its own and the choice is one step further in, in
 * the body of that object: the {@code exists} of a file is a name, while what
 * stands behind it is either of two. The walk to the attribute goes behind
 * that body, so whatever it passed on the way answers for it.</p>
 *
 * <p>The nearest body wins. A walk may go behind several, and the one closest
 * to the question is the one that says the most about it, the way a decorator
 * says less about an object than the object itself. A walk that goes behind
 * none leaves the read as it was: the {@code func} of a {@code Φ.tuple.eachi}
 * is a void of that very object, so whatever stands behind its {@code φ} was
 * never asked and has nothing to say about the {@code func}.</p>
 *
 * @since 0.68.0
 */
final class Borne {

    /**
     * The arms already known, by the locator of the dispatch.
     */
    private final Map<String, Collection<String>> found;

    /**
     * What every name turns out to be, from {@link Ends}.
     */
    private final Map<String, String> names;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     *
     * @param arms The arms already known, by the locator of the dispatch
     * @param ends What every name turns out to be, from {@link Ends}
     * @param provided What the types certainly have
     */
    Borne(
        final Map<String, Collection<String>> arms,
        final Map<String, String> ends,
        final Provided provided
    ) {
        this.found = arms;
        this.names = ends;
        this.owned = provided;
    }

    /**
     * The objects the receiver of this dispatch comes back with.
     *
     * @param dispatch The dispatch
     * @return The locators of the objects, empty when the receiver comes back
     *  with one object or with none anybody named
     */
    Collection<String> arms(final Site dispatch) {
        final String bearer = dispatch.bearer();
        Collection<String> arms = this.found.getOrDefault(bearer, Collections.emptyList());
        if (arms.isEmpty()) {
            for (final String body : this.passed(
                this.names.getOrDefault(bearer, bearer), dispatch.name()
            )) {
                if (this.found.containsKey(body)) {
                    arms = this.found.get(body);
                    break;
                }
            }
        }
        return arms;
    }

    private Collection<String> passed(final String type, final String name) {
        final Collection<String> bodies = new ArrayList<>(0);
        final Collection<String> seen = new HashSet<>(0);
        String walked = type;
        while (!walked.isEmpty() && this.owned.here(walked, name).isEmpty()
            && seen.add(walked)) {
            final String body = this.owned.body(walked);
            if (!body.isEmpty()) {
                bodies.add(body);
            }
            walked = this.owned.behind(walked);
        }
        return bodies;
    }
}
