/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

/**
 * The names a call hands arguments to while nobody can say whose formation
 * that call takes.
 *
 * <p>{@code ^.child.run s at caps k} on a void {@code child} fills the voids
 * of some {@code run}, and the passes never learn whose: {@link Bound} binds
 * nothing for a call it cannot place, so the call is missing from every list
 * of fillings built from the binds. Not knowing whose voids it fills is not
 * knowing that nobody's are, so every void of a formation by that name is
 * filled by somebody the tables cannot see (#9062). The match is by name and
 * coarse on purpose: a sharper one would need the receiver, and the receiver
 * is exactly what nobody knows here.</p>
 *
 * <p>This is asked in three places, by the passes before they strike an arm,
 * by {@link Promoted} before it names a void after its one filling, and by
 * {@link Witnessed} once every pass is over, and each of them hands
 * {@link Fillings} what it learns. Three answers to one question are how
 * the census came to call a void filled by one caller while another filled
 * it out of sight (#9006).</p>
 *
 * @since 0.71.0
 * @todo #9006:90min Match a call nobody can place by more than its name.
 *  On eo-runtime the name alone opens 259 voids, and 115 of them lose the one
 *  member they were named after: a single {@code x.lt y} on a receiver the
 *  passes never settle opens the {@code b} of {@code lt} in every one of the
 *  eight integers, and {@code run} alone opens 60. Counting the arguments the
 *  call hands over, or the formations its receiver could still turn out to
 *  be, would leave most of those voids named. Until then the named share is
 *  88.2 where it was 89.0, and none of the lost names was ever proven.
 */
final class Unplaced {

    /**
     * Every dispatch and read of the program.
     */
    private final Collection<Site> all;

    /**
     * The positional arguments of every application.
     */
    private final Map<String, List<String>> args;

    /**
     * The named arguments of every application.
     */
    private final Map<String, Map<String, String>> named;

    /**
     * The calls whose formation is known, each by the object it makes.
     */
    private final Collection<String> placed;

    /**
     * Ctor.
     *
     * @param sites Every dispatch and read of the program
     * @param arguments The positional arguments of every application
     * @param bindings The named arguments of every application
     * @param bound The objects made by the calls whose formation is known
     */
    Unplaced(
        final Collection<Site> sites,
        final Map<String, List<String>> arguments,
        final Map<String, Map<String, String>> bindings,
        final Collection<String> bound
    ) {
        this.all = sites;
        this.args = arguments;
        this.named = bindings;
        this.placed = bound;
    }

    /**
     * The names such calls hand arguments to.
     *
     * <p>A call made on a void something fills is not among them. It is a call
     * on whatever fills that void, and {@link Bound} hands its arguments on to
     * the voids of every one of those, so it binds nothing on its own row and
     * is in sight all the same. Counting it here made the very void it is
     * made on filled out of sight, the void was then named after nothing,
     * and the call stayed where nobody could place it.</p>
     *
     * @param ends What every object is a copy of in the end, from {@link Ends}
     * @param filled The voids something in sight fills
     * @return The attribute names, as the dispatches spell them
     */
    Collection<String> names(final Map<String, String> ends, final Collection<String> filled) {
        final Collection<String> found = new HashSet<>(0);
        for (final Site dispatch : this.all) {
            if (!this.placed.contains(dispatch.made())
                && !filled.contains(ends.getOrDefault(dispatch.bearer(), dispatch.bearer()))
                && (!this.args.getOrDefault(dispatch.made(), Collections.emptyList()).isEmpty()
                || !this.named.getOrDefault(dispatch.made(), Collections.emptyMap()).isEmpty())) {
                found.add(dispatch.name());
            }
        }
        return found;
    }
}
