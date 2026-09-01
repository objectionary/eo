/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * What every dispatch of a program turns out to be.
 *
 * <p>A dispatch takes a name from an object, so when that object's type is
 * known and it has such an attribute, the dispatch <em>is</em> that attribute:
 * a copy of it, which is what a pair in {@link Links} means. The body of
 * {@code and} is {@code .if} taken from {@code Φ.bool}, {@code if} is a member
 * of that package, so the body is a copy of {@code Φ.bool.if}. Where the
 * attribute is looked for is {@link Provided}'s business — the object itself,
 * its package, or behind its {@code φ}.</p>
 *
 * <p>Nothing is guessed. A receiver whose type nothing describes is left
 * alone, and so is a receiver that is a void: {@code x.next} inside an object
 * that takes {@code x} is one object in the text and a different one for every
 * caller, and the pair that would settle it belongs to the call site rather
 * than here. A dispatch already spoken for is left alone too, since a pass is
 * only ever asked for what the last one could not answer.</p>
 *
 * @since 0.68.0
 */
final class Dispatched {

    /**
     * The provides table.
     */
    private final XML given;

    /**
     * Every dispatch of the program.
     */
    private final Collection<Site> all;

    /**
     * The arguments of every application, from {@link Given}.
     */
    private final Map<String, List<String>> args;

    /**
     * The arguments of every application bound by name, from {@link Given}.
     */
    private final Map<String, Map<String, String>> named;

    /**
     * What every dispatch takes its attribute from, from {@link Xmirs}.
     */
    private final Map<String, String> receivers;

    /**
     * The locator of every void this pass may look into.
     */
    private final Collection<String> hollows;

    /**
     * Ctor.
     * @param provides The provides table
     * @param dispatches Every dispatch of the program
     * @param arguments The arguments of every application, from {@link Given}
     * @param bindings The arguments of every application bound by name
     * @param taken What every dispatch takes its attribute from
     * @param voids The locator of every void this pass may look into, empty
     *  when it may look into none
     */
    Dispatched(
        final XML provides,
        final Collection<Site> dispatches,
        final Map<String, List<String>> arguments,
        final Map<String, Map<String, String>> bindings,
        final Map<String, String> taken,
        final Collection<String> voids
    ) {
        this.given = provides;
        this.all = dispatches;
        this.args = arguments;
        this.named = bindings;
        this.receivers = taken;
        this.hollows = voids;
    }

    /**
     * The pairs that follow from what is known, beyond what is known already.
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The dispatches answered this time, each against the attribute it
     *  turns out to be, empty when nothing further can be answered
     */
    Map<String, String> answers(final Map<String, String> pairs) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(this.given, names, this.hollows);
        final Filled filled = new Filled(
            pairs,
            owned,
            new Bound(this.args, this.named, this.receivers, pairs, owned).all()
        );
        final Map<String, String> found = new HashMap<>(0);
        for (final Site dispatch : this.all) {
            final String made = dispatch.made();
            if (!pairs.containsKey(made)) {
                final String bearer = dispatch.bearer();
                final String kept = filled.instead(
                    owned.attribute(names.getOrDefault(bearer, bearer), dispatch.name()),
                    bearer
                );
                if (!kept.isEmpty() && !kept.equals(made)) {
                    found.put(made, kept);
                }
            }
        }
        return found;
    }
}
