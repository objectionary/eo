/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * The voids that hand an argument back instead of answering for themselves.
 *
 * <p>A formation whose whole body is one of its own voids owns nothing: the
 * {@code [? >> left ? >> right]} that {@code Φ.true} puts into
 * {@code Φ.bool.if} answers with its {@code left} and the one {@code Φ.false}
 * puts there answers with its {@code right}, and neither has a name of its own
 * to offer. A void filled with nothing but formations of that shape therefore
 * owes nothing either, however much the program reads off a call on it, since
 * every one of those names belongs to an argument and {@link Branched} is what
 * says which. Where the arguments agree on nothing the names have nowhere to
 * go, and nowhere is where they go.</p>
 *
 * <p>Every filling has to be of that shape for the void to be one of these. One
 * formation that binds a body of its own answers names like any other object,
 * and a caller that lands on it is owed them.</p>
 *
 * <p>Standing in front of a void is not enough either: the formation must keep
 * nothing else. {@code Φ.directory} hands its answers to the {@code file} it
 * was built from, yet it binds {@code made} and {@code deleted} and a dozen
 * more names of its own, so a caller that reads a name off a directory is owed
 * it by the directory, and the demand belongs where it is written. What a
 * picker has instead is two voids, a {@code φ} pointing at one of them, and
 * nothing anybody could ask it for.</p>
 *
 * <p>A body nobody bound is not one of those voids, however much it looks like
 * one. {@code Φ.number} keeps a {@code φ} the table writes down as a void,
 * since what a number stands in front of is nowhere in the source, and a
 * number still answers every name a number answers. What makes a formation
 * hand an argument back is that its body is a void a caller <em>fills</em>,
 * and nobody fills a {@code φ}.</p>
 *
 * @since 0.73.0
 */
final class Relayed {

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * What went into every void, by the locator of the void, from
     * {@link Said#puts()}.
     */
    private final Map<String, Collection<String>> given;

    /**
     * The name every type goes by.
     */
    private final Map<String, String> names;

    /**
     * Ctor.
     *
     * @param provided What the types certainly have
     * @param puts What went into every void, from {@link Said#puts()}
     * @param aliases The name every type goes by, from {@link Ends}
     */
    Relayed(
        final Provided provided,
        final Map<String, Collection<String>> puts,
        final Map<String, String> aliases
    ) {
        this.owned = provided;
        this.given = puts;
        this.names = aliases;
    }

    /**
     * The voids nothing can be asked of.
     *
     * @return The locators of the voids every filling of which hands back one
     *  of the arguments it was called with
     */
    Collection<String> all() {
        final Collection<String> found = new LinkedHashSet<>(0);
        for (final Map.Entry<String, Collection<String>> hollow : this.given.entrySet()) {
            if (!hollow.getValue().isEmpty() && hollow.getValue().stream().allMatch(this::hands)) {
                found.add(hollow.getKey());
            }
        }
        return found;
    }

    private boolean hands(final String filling) {
        final String type = this.names.getOrDefault(filling, filling);
        final String body = this.owned.behind(type);
        final String name = body.substring(body.lastIndexOf('.') + 1);
        return !body.isEmpty() && !"φ".equals(name)
            && body.equals(this.owned.named(type, name))
            && this.owned.bare(type);
    }
}
