/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * The voids nothing fills, said once no run of the passes disagrees.
 *
 * <p>An arm that reads a void nothing fills is dead, but that is a claim of
 * absence, and a claim of absence asked while the passes still add fillings
 * is asked too soon. So it is asked of the pairs a whole run ended with, a
 * run that struck nothing. That run does not know the most, though: a call
 * that is a choice of two arms hands its arguments on to fewer voids than a
 * call settled on one, so once an arm is struck a void the list calls empty
 * may be filled after all, as the {@code message} of the formation an
 * {@code i8} test hands to {@code div} is. Such a void comes off the list and
 * the passes run again, until a run fills nothing on it. The list only ever
 * shrinks, so that ends, and the rows written from it agree with the census
 * (#8981).</p>
 *
 * @since 0.71.0
 */
final class Emptied {

    /**
     * The dispatches, striking nothing.
     */
    private final Dispatched made;

    /**
     * What the voids the program fills one way turn out to be.
     */
    private final Promoted more;

    /**
     * Ctor.
     *
     * @param dispatched The dispatches, striking nothing
     * @param promoted What the voids the program fills one way turn out to be
     */
    Emptied(final Dispatched dispatched, final Promoted promoted) {
        this.made = dispatched;
        this.more = promoted;
    }

    /**
     * The voids nothing fills.
     *
     * @param pairs The pairs to run the passes from
     * @return The locators of the voids, none of which a run striking every
     *  arm that reads one of them fills
     */
    Collection<String> from(final Map<String, String> pairs) {
        final Collection<String> found = new LinkedHashSet<>(
            this.made.empty(new Settled(this.made, this.more).from(pairs))
        );
        Collection<String> left = this.left(found, pairs);
        while (!left.containsAll(found)) {
            found.retainAll(left);
            left = this.left(found, pairs);
        }
        return found;
    }

    private Collection<String> left(
        final Collection<String> empty, final Map<String, String> pairs
    ) {
        final Dispatched strict = this.made.striking(empty);
        return strict.empty(new Settled(strict, this.more).from(pairs));
    }
}
