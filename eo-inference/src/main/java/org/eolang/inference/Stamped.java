/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The object a name written after a dot hands its receiver to.
 *
 * <p>{@code a.b} takes {@code b} off {@code a}, and the runtime stamps the
 * owner on whatever {@code b} is a copy of, as long as that copy has no
 * receiver of its own yet. So the chain of copies is walked from {@code b},
 * and the walk stops at the first object that settles it. A formation that
 * declares {@code ^} settles it one way, since that is where the receiver
 * goes: {@code board.raised}, where the board says {@code flag > raised},
 * fills the {@code ^} of that {@code flag}. A copy that was itself taken off
 * something settles it the other way, since it got its receiver where it was
 * written: {@code os.is-windows} is a copy of {@code name.contains}, and the
 * {@code os} goes nowhere (#8955).</p>
 *
 * <p>A bare name of the formation's own does not settle it, though. The
 * {@code get} of a {@code chunk} is a copy of its {@code read}, which it takes
 * off whatever it is read from, so {@code c.get} fills the {@code ^} of that
 * {@code read} with the {@code c}.</p>
 *
 * <p>A formation declaring {@code ^} is asked about first, because the parser
 * writes that {@code ^} where it writes the receiver of a dispatch, and
 * {@link Xmirs} finds the two the same way.</p>
 *
 * <p>A formation written as an argument is stamped the same way, by whatever
 * holds the void it was put in, which is the application that put it there.</p>
 *
 * @since 0.76.0
 */
final class Stamped {

    /**
     * The pairs, each name against the one it is a copy of.
     */
    private final Map<String, String> pairs;

    /**
     * What every dispatch takes its attribute from, from {@link Taken}.
     */
    private final Map<String, String> receivers;

    /**
     * What the types certainly have.
     */
    private final Provided owned;

    /**
     * Ctor.
     *
     * @param links The pairs, each name against the one it is a copy of
     * @param taken What every dispatch takes its attribute from
     * @param provided What the types certainly have
     */
    Stamped(
        final Map<String, String> links, final Map<String, String> taken,
        final Provided provided
    ) {
        this.pairs = links;
        this.receivers = taken;
        this.owned = provided;
    }

    /**
     * Owners of dispatched attributes, retaining applied body bindings.
     *
     * @param sites The dispatches
     * @return Attribute owners by the application locator
     */
    Map<String, String> bearers(final Collection<Site> sites) {
        final Map<String, String> found = new HashMap<>(0);
        final Map<String, String> names = new Ends(this.pairs).names();
        for (final Site site : sites) {
            String bearer = site.bearer();
            final Collection<String> seen = new HashSet<>(0);
            while (!bearer.isEmpty() && seen.add(bearer)) {
                final String type = names.getOrDefault(bearer, bearer);
                if (!this.owned.here(type, site.name()).isEmpty()) {
                    found.put(site.made(), bearer);
                    break;
                }
                bearer = this.owned.body(type);
            }
        }
        return found;
    }

    /**
     * The object the receiver of this dispatch lands on.
     *
     * @param dispatch The locator of a dispatch written after a dot
     * @return The locator of the object whose receiver it fills, or an empty
     *  string when the copy it reaches has a receiver already
     */
    String names(final String dispatch) {
        return this.lands(this.pairs.getOrDefault(dispatch, ""));
    }

    /**
     * The object the receiver lands on when the dispatch comes back as this.
     *
     * <p>A dispatch that comes back with one of several objects hands its
     * receiver to whichever of them it comes back with, and the walk down the
     * chain of copies starts at that arm, where it starts at the pair of a
     * dispatch that comes back with one (#8885).</p>
     *
     * @param arm The locator of what the dispatch comes back as
     * @return The locator of the object whose receiver it fills, or an empty
     *  string when the copy it reaches has a receiver already
     */
    String lands(final String arm) {
        final Collection<String> seen = new HashSet<>(0);
        String walked = arm;
        while (this.owned.receiver(walked).isEmpty() && !this.dotted(walked)
            && this.pairs.containsKey(walked) && seen.add(walked)) {
            walked = this.pairs.get(walked);
        }
        final String found;
        if (this.owned.receiver(walked).isEmpty() && this.dotted(walked)) {
            found = "";
        } else if (this.owned.receiver(walked).isEmpty()) {
            found = new Ends(this.pairs).name(walked);
        } else {
            found = walked;
        }
        return found;
    }

    /**
     * Give every formation an application puts into a void its receiver.
     *
     * <p>A formation written as an argument belongs to nobody, and the
     * {@code ^} it declares is empty until it is read off the void it was put
     * in. The runtime stamps it then with the object that holds the void, as
     * it does with any attribute, and that object is the application or a
     * copy of it: {@code x [^]} fills the {@code ^} of that {@code [^]} with
     * the {@code x}, whoever reads its {@code y}. A copy that points at a
     * formation got its receiver where it was written and is not told, a
     * receiver filled already stays, and one that came by way of a relay is a
     * relay too (#9359).</p>
     *
     * @param found What every application fills, to add the receivers to
     * @param relays What every application fills by way of a relay, to add
     *  the receivers that came that way to
     */
    void adopt(
        final Map<String, Map<String, String>> found,
        final Map<String, Collection<String>> relays
    ) {
        final Map<String, Map<String, String>> adopted = new HashMap<>(0);
        for (final Map.Entry<String, Map<String, String>> filled : found.entrySet()) {
            for (final Map.Entry<String, String> fill : filled.getValue().entrySet()) {
                final String hollow = this.owned.receiver(fill.getValue());
                if (!hollow.isEmpty() && !fill.getKey().endsWith(".ρ")
                    && !filled.getValue().containsKey(hollow)) {
                    adopted.computeIfAbsent(filled.getKey(), key -> new LinkedHashMap<>(1))
                        .put(hollow, filled.getKey());
                    if (relays.getOrDefault(filled.getKey(), Collections.emptySet())
                        .contains(fill.getKey())) {
                        relays.get(filled.getKey()).add(hollow);
                    }
                }
            }
        }
        adopted.forEach((application, given) -> found.get(application).putAll(given));
    }

    private boolean dotted(final String name) {
        return this.receivers.getOrDefault(name, "").equals(name.concat(".ρ"));
    }
}
