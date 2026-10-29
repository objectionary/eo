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
     * The run that no void disagrees with.
     *
     * <p>The pairs of that run come back with it, and not only the voids it
     * left unfilled, because whoever asks for the voids goes on to settle the
     * pairs against the dispatches that strike them — which is the run this
     * has just ended on. Asking for it again cost 85 of the 467 passes a
     * build of {@code eo-runtime} makes (#9648).</p>
     *
     * @param pairs The pairs to run the passes from
     * @return The last run, whose voids are the ones nothing fills
     */
    Struck from(final Map<String, String> pairs) {
        final Collection<String> found = new LinkedHashSet<>(
            this.made.empty(new Settled(this.made, this.more).from(pairs))
        );
        Struck struck = this.struck(found, pairs);
        while (!struck.empty().containsAll(found)) {
            found.retainAll(struck.empty());
            struck = this.struck(found, pairs);
        }
        return struck;
    }

    private Struck struck(
        final Collection<String> empty, final Map<String, String> pairs
    ) {
        final Dispatched strict = this.made.striking(empty);
        final Map<String, String> settled = new Settled(strict, this.more).from(pairs);
        return new Struck(strict, settled, strict.empty(settled));
    }
}
