/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;

/**
 * The receivers a dispatch on a void fills, by way of what the void holds.
 *
 * <p>A dispatch whose chain of copies ends on a name taken off a void, such as
 * {@code tail.ends-with} where one caller puts a {@code text} into
 * {@code tail} and another a {@code rope}, declares no receiver of its own.
 * It is still a dispatch into the {@code ends-with} of every formation the
 * void is seen to hold, so the name is taken off each of them, and the
 * receiver each of them declares is filled. A dispatch that filled nothing
 * dropped out of the census of that receiver without a trace, and a census
 * left with one member settled the void as that member, which the
 * {@code tail} is not (#8960).</p>
 *
 * <p>What goes in is what the caller put into the void, so the receiver of
 * the {@code ends-with} of a {@code text} hears of the {@code text} and not
 * of the {@code rope}. Where no one caller can be named for it, because two
 * formations reach the same receiver or the void holds a name nobody has
 * looked into yet, the receiver is filled with the void itself, which brings
 * along everything the void holds. One member too many keeps a census from
 * settling; one too few settles it on a lie.</p>
 *
 * <p>A void may be filled with another void, and whatever fills that one
 * fills this one too, a hop further along. Only a name taken right off the
 * void is walked. A longer one, such as {@code s.as-bytes.size.gt}, is a
 * dispatch on the answer of another dispatch that nobody has settled yet,
 * and walking it from the void through every formation the void holds lands
 * on receivers the program never reaches: the {@code run} of a regex filled
 * with a boolean. And the receiver has to be the void itself, as the
 * {@code tail} is: a receiver that is some other name, and only lands under
 * a void by way of what its attribute is looked up as, is somebody else's
 * question (#8955).</p>
 *
 * <p>This is written into the rows and not told to the passes. A call that
 * fills a void of a formation is taken by {@link Filled} to be a call on
 * that formation, and {@code b.as-bytes}, where {@code b} holds a
 * {@code bytes} for one caller and an {@code i64} for another, is a call on
 * neither: telling the passes turned it into the {@code as-bytes} of a
 * {@code bytes} for both.</p>
 *
 * @since 0.74.0
 */
final class Hung {

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * What every application fills, by the locator of the application.
     */
    private final Map<String, Map<String, String>> fills;

    /**
     * What every dispatch takes its attribute from, from {@link Taken}.
     */
    private final Map<String, String> receivers;

    /**
     * Ctor.
     *
     * @param provided What the types certainly have
     * @param links The pairs, each name against the one it is a copy of
     * @param filled What every application fills, from {@link Bound}
     * @param taken What every dispatch takes its attribute from
     */
    Hung(
        final Provided provided, final Map<String, String> links,
        final Map<String, Map<String, String>> filled, final Map<String, String> taken
    ) {
        this.owned = provided;
        this.pairs = links;
        this.fills = filled;
        this.receivers = taken;
    }

    /**
     * What every application fills, with the receivers every dispatch on a
     * void fills put in.
     *
     * @param bases What every dispatch is a copy of, by the locator of the
     *  dispatch
     * @return The objects the voids hold, by the locator of the void, by the
     *  locator of the application
     */
    Map<String, Map<String, String>> all(final Map<String, String> bases) {
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        this.fills.forEach(
            (application, filled) -> found.put(application, new LinkedHashMap<>(filled))
        );
        this.hung(bases).forEach(
            (dispatch, hung) -> hung.forEach(
                found.computeIfAbsent(dispatch, key -> new LinkedHashMap<>(1))::putIfAbsent
            )
        );
        return found;
    }

    /**
     * The voids every application fills by way of what a void was seen to
     * hold, the receivers every dispatch on a void fills among them.
     *
     * @param bases What every dispatch is a copy of, by the locator of the
     *  dispatch
     * @param relays The voids every application fills by way of a relay, from
     *  {@link Bound}
     * @return The voids, by the locator of the application
     */
    Map<String, Collection<String>> relays(
        final Map<String, String> bases, final Map<String, Collection<String>> relays
    ) {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        relays.forEach((application, hollows) -> found.put(application, new HashSet<>(hollows)));
        this.hung(bases).forEach(
            (dispatch, hung) -> found.computeIfAbsent(dispatch, key -> new HashSet<>(1))
                .addAll(hung.keySet())
        );
        return found;
    }

    private Map<String, Map<String, String>> hung(final Map<String, String> bases) {
        final Map<String, Map<String, String>> sources = this.sources();
        final Ends ends = new Ends(this.pairs);
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        for (final Map.Entry<String, String> base : bases.entrySet()) {
            final String receiver = this.receivers.getOrDefault(base.getKey(), "");
            final Map<String, String> hung = this.hanging(
                base.getValue(), receiver, ends.name(receiver), sources
            );
            if (!hung.isEmpty()) {
                found.put(base.getKey(), hung);
            }
        }
        return found;
    }

    private Map<String, String> hanging(
        final String base, final String receiver, final String end,
        final Map<String, Map<String, String>> sources
    ) {
        final String root = this.owned.root(base);
        final Map<String, String> found = new LinkedHashMap<>(0);
        if (!root.isEmpty() && root.equals(end) && base.lastIndexOf('.') == root.length()) {
            final Map<String, String> held = this.formations(root, sources, new HashSet<>(0));
            final boolean unknown = held.keySet().stream().anyMatch(this.owned::hollow);
            for (final Map.Entry<String, Collection<String>> hollow
                : this.reached(base.substring(root.length() + 1), held.keySet()).entrySet()) {
                if (unknown || hollow.getValue().size() > 1) {
                    found.put(hollow.getKey(), receiver);
                } else {
                    found.put(hollow.getKey(), held.get(hollow.getValue().iterator().next()));
                }
            }
        }
        return found;
    }

    private Map<String, Collection<String>> reached(
        final String name, final Collection<String> formations
    ) {
        final Map<String, Collection<String>> found = new LinkedHashMap<>(0);
        for (final String formation : formations) {
            final String hollow = this.owned.receiver(this.owned.attribute(formation, name));
            if (!hollow.isEmpty()) {
                found.computeIfAbsent(hollow, key -> new LinkedHashSet<>(1)).add(formation);
            }
        }
        return found;
    }

    private Map<String, String> formations(
        final String hollow, final Map<String, Map<String, String>> sources,
        final Collection<String> seen
    ) {
        final Map<String, String> found = new LinkedHashMap<>(0);
        if (seen.add(hollow)) {
            for (final Map.Entry<String, String> filler
                : sources.getOrDefault(hollow, Collections.emptyMap()).entrySet()) {
                if (filler.getKey().equals(this.owned.root(filler.getKey()))) {
                    this.formations(filler.getKey(), sources, seen).forEach(found::putIfAbsent);
                } else {
                    found.putIfAbsent(filler.getKey(), filler.getValue());
                }
            }
        }
        return found;
    }

    private Map<String, Map<String, String>> sources() {
        final Ends ends = new Ends(this.pairs);
        final Map<String, Map<String, String>> found = new LinkedHashMap<>(0);
        for (final Map<String, String> filled : this.fills.values()) {
            for (final Map.Entry<String, String> fill : filled.entrySet()) {
                found.computeIfAbsent(fill.getKey(), key -> new LinkedHashMap<>(0))
                    .putIfAbsent(ends.name(fill.getValue()), fill.getValue());
            }
        }
        return found;
    }
}
