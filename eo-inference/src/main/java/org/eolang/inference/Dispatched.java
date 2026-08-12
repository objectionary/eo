/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashMap;
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
    private final Collection<XML> all;

    /**
     * Ctor.
     * @param provides The provides table
     * @param dispatches Every dispatch of the program
     */
    Dispatched(final XML provides, final Collection<XML> dispatches) {
        this.given = provides;
        this.all = dispatches;
    }

    /**
     * The pairs that follow from what is known, beyond what is known already.
     * @param pairs The pairs, each name against the one it is a copy of
     * @return The dispatches answered this time, each against the attribute it
     *  turns out to be, empty when nothing further can be answered
     */
    Map<String, String> answers(final Map<String, String> pairs) {
        final Map<String, String> names = new Ends(pairs).names();
        final Provided owned = new Provided(new Ungrouped(this.given, names).rows(), names);
        final Map<String, String> found = new HashMap<>(0);
        for (final XML dispatch : this.all) {
            final String made = dispatch.xpath("@loc").get(0);
            if (!pairs.containsKey(made)) {
                final String bearer = dispatch.xpath("o[not(@as)][1]/@loc").get(0);
                final String kept = owned.attribute(
                    names.getOrDefault(bearer, bearer),
                    dispatch.xpath("@base").get(0).substring(1)
                );
                if (!kept.isEmpty()) {
                    found.put(made, kept);
                }
            }
        }
        return found;
    }
}
