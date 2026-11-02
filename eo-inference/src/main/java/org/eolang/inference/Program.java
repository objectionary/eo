/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * What {@code facts.xml} says about a program, and nothing worked out.
 *
 * <p>Four parts (design doc 2): the formations with their attributes, every
 * name read and what it is read off, what each chain starts from, and each
 * call with its arguments in order. A <b>place</b> is a spot in the source,
 * named by a path from {@code Φ} down, and every place is answered in the
 * formation right around it, the nearest one its name starts with.</p>
 *
 * @since 0.76.0
 */
final class Program {

    /**
     * The formations, by name, in the order of the facts.
     */
    private final Map<String, Formation> forms;

    /**
     * The reads, by place.
     */
    private final Map<String, Read> reads;

    /**
     * What each chain starts from, by place.
     */
    private final Map<String, List<Head>> heads;

    /**
     * The arguments of each call, in order, by place.
     */
    private final Map<String, List<Argument>> calls;

    /**
     * The places of the arguments of each call, worked out once.
     */
    private final Map<String, List<String>> locs;

    /**
     * The formation right around each place, worked out once.
     */
    private final Map<String, String> around;

    /**
     * The places answered in each formation, worked out once.
     */
    private final Map<String, List<String>> bodies;

    /**
     * Ctor.
     *
     * @param formations The formations, by name, in the order of the facts
     * @param named The reads, by place
     * @param starts What each chain starts from, by place
     * @param applied The arguments of each call, by place
     */
    Program(
        final Map<String, Formation> formations, final Map<String, Read> named,
        final Map<String, List<Head>> starts, final Map<String, List<Argument>> applied
    ) {
        this.forms = formations;
        this.reads = named;
        this.heads = starts;
        this.calls = applied;
        this.locs = new HashMap<>(0);
        this.around = new HashMap<>(0);
        this.bodies = new LinkedHashMap<>(0);
    }

    /**
     * The names of the formations, in the order of the facts.
     *
     * @return The names
     */
    Collection<String> formations() {
        return this.forms.keySet();
    }

    /**
     * Whether a name is a formation.
     *
     * @param name The name
     * @return TRUE if it is
     */
    boolean formation(final String name) {
        return this.forms.containsKey(name);
    }

    /**
     * The formation of this name.
     *
     * @param name The name
     * @return The formation
     */
    Formation form(final String name) {
        final Formation form = this.forms.get(name);
        if (form == null) {
            throw new IllegalStateException(
                String.format("%s is not a formation of the facts", name)
            );
        }
        return form;
    }

    /**
     * Whether a place reads a name.
     *
     * @param place The place
     * @return TRUE if it does
     */
    boolean reads(final String place) {
        return this.reads.containsKey(place);
    }

    /**
     * The read at a place.
     *
     * @param place The place
     * @return The read
     */
    Read read(final String place) {
        return this.reads.get(place);
    }

    /**
     * Whether a place starts a chain.
     *
     * @param place The place
     * @return TRUE if it does
     */
    boolean starts(final String place) {
        return this.heads.containsKey(place);
    }

    /**
     * What a chain at a place starts from.
     *
     * @param place The place
     * @return The heads, none when the place starts no chain
     */
    List<Head> heads(final String place) {
        return this.heads.getOrDefault(place, Collections.emptyList());
    }

    /**
     * Whether a place is a call.
     *
     * @param place The place
     * @return TRUE if it is
     */
    boolean calls(final String place) {
        return this.calls.containsKey(place);
    }

    /**
     * The arguments of the call at a place, in order.
     *
     * @param place The place
     * @return The arguments, none when it is not a call
     */
    List<Argument> args(final String place) {
        return this.calls.getOrDefault(place, Collections.emptyList());
    }

    /**
     * The places of the arguments of the call at a place, in order.
     *
     * @param place The place
     * @return The places, none when it is not a call
     */
    List<String> locs(final String place) {
        final List<String> found;
        if (this.calls.containsKey(place)) {
            found = this.locs.computeIfAbsent(
                place, key -> this.calls.get(key).stream().map(Argument::loc).toList()
            );
        } else {
            found = Collections.emptyList();
        }
        return found;
    }

    /**
     * Whether a name is a place the facts answer: a read, a chain or a call.
     *
     * @param name The name
     * @return TRUE if it is
     */
    boolean place(final String name) {
        return this.reads.containsKey(name) || this.heads.containsKey(name)
            || this.calls.containsKey(name);
    }

    /**
     * The formation right around a place: the longest name in front of it,
     * cut at a dot, that is a formation.
     *
     * @param place The place
     * @return The formation, empty when none is around
     */
    String nearest(final String place) {
        return this.around.computeIfAbsent(
            place,
            key -> {
                String found = "";
                int cut = key.lastIndexOf('.');
                while (cut > 0) {
                    final String pre = key.substring(0, cut);
                    if (this.forms.containsKey(pre)) {
                        found = pre;
                        break;
                    }
                    cut = pre.lastIndexOf('.');
                }
                return found;
            }
        );
    }

    /**
     * The places answered in each copy of a formation, those of top-level
     * code under the empty name, in the order of the facts.
     *
     * @param form The formation, or empty for top-level code
     * @return The places
     */
    List<String> body(final String form) {
        if (this.bodies.isEmpty()) {
            final Set<String> places = new LinkedHashSet<>(this.reads.keySet());
            places.addAll(this.heads.keySet());
            places.addAll(this.calls.keySet());
            for (final String place : places) {
                if (!this.forms.containsKey(place)) {
                    this.bodies.computeIfAbsent(
                        this.nearest(place), key -> new ArrayList<>(1)
                    ).add(place);
                }
            }
            this.bodies.putIfAbsent("", new ArrayList<>(0));
        }
        return this.bodies.getOrDefault(form, Collections.emptyList());
    }
}
