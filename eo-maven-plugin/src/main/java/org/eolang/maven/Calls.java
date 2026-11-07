/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;
import org.eolang.inference.Annotation;

/**
 * What the calls of the program put into the voids of the copies they make.
 *
 * <p>A call puts its arguments into the voids of the formation its head
 * copies, counting the voids other than {@code ρ} in the order they are
 * written. With {@code [cde msg] > status}, the call {@code status 0 "done"}
 * at {@code Φ.app.φ} puts the object at {@code Φ.app.φ.α0} into
 * {@code cde} and the one at {@code Φ.app.φ.α1} into {@code msg}. An
 * argument bound by name goes into the void it names. The code tells where an
 * argument goes only when its call fills every void of the formation once,
 * so a call with fewer or more arguments than voids tells nothing here. A
 * head whose copy already holds something in a void has no room for that
 * many arguments, so such a call fails before anyone reads its voids.</p>
 *
 * <p>An atom is a call that Java makes. The type it says it returns, such as
 * {@code Φ.posix.status-return(Φ.number,Φ.string)}, lists in brackets what
 * Java puts into each void, in the same order.</p>
 *
 * @since 0.76.0
 */
final class Calls {

    /**
     * The voids of each formation other than {@code ρ}, in the order they
     * are written, by the formation.
     */
    private final Map<String, List<String>> voids;

    /**
     * The arguments of each call, by the place of the call: the place of each
     * argument, by its position, such as {@code α0}, or by the name of the
     * void it is bound to.
     */
    private final Map<String, Map<String, String>> args;

    /**
     * Ctor.
     *
     * @param holes The voids of each formation other than {@code ρ}, in order
     * @param bindings The arguments of each call, by the place of the call
     */
    Calls(final Map<String, List<String>> holes, final Map<String, Map<String, String>> bindings) {
        this.voids = holes;
        this.args = bindings;
    }

    /**
     * The copy that a place makes of what its head copies.
     *
     * @param place The place
     * @param head What the head of the place copies
     * @param host The copy the place is written in
     * @return A new copy when the place is a call, or the head itself
     */
    Copy copy(final String place, final Copy head, final Copy host) {
        final Copy found;
        if (this.args.containsKey(place)) {
            found = head.applied(place, host);
        } else {
            found = head;
        }
        return found;
    }

    /**
     * The place of the argument that a call puts into a void.
     *
     * @param call The place of the call
     * @param formation The formation the call copies
     * @param name The name of the void
     * @return The place of the argument, or nothing when the call does not
     *  fill every void of the formation once
     */
    Optional<String> argument(final String call, final String formation, final String name) {
        final List<String> holes = this.voids.getOrDefault(formation, Collections.emptyList());
        final Map<String, String> bound = this.args.getOrDefault(call, Collections.emptyMap());
        final int pos = holes.indexOf(name);
        final Optional<String> found;
        if (pos < 0 || holes.size() != bound.size()) {
            found = Optional.empty();
        } else {
            found = Stream.of(String.format("α%d", pos), name)
                .filter(bound::containsKey)
                .map(bound::get)
                .findFirst();
        }
        return found;
    }

    /**
     * The type that Java puts into a void of a copy an atom returns.
     *
     * @param type The type the atom says it returns
     * @param name The name of the void
     * @return The type in the brackets, or nothing when they leave the void
     *  out
     */
    Optional<String> member(final Annotation type, final String name) {
        final List<Annotation> members = type.members();
        final int pos = this.voids.getOrDefault(
            type.head(), Collections.emptyList()
        ).indexOf(name);
        final Optional<String> found;
        if (pos >= 0 && pos < members.size()) {
            found = Optional.of(members.get(pos).toString());
        } else {
            found = Optional.empty();
        }
        return found;
    }
}
