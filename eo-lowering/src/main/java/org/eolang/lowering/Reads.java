/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Stream;

/**
 * The voids a protocol reads, and the ones its own block declares.
 *
 * <p>It takes a protocol and answers two sets of indexes: everything the
 * protocol reaches, and the part of that this block declares rather than
 * leaving to an arm below or taking from a block above. A void only one
 * arm of a fork touches belongs to that arm, so an argument a guard
 * protects is never forced when the guard sends the run the other way.</p>
 *
 * @since 0.76.0
 */
final class Reads {

    /**
     * The protocol.
     */
    private final Protocol protocol;

    /**
     * Ctor.
     *
     * @param proto The protocol
     */
    Reads(final Protocol proto) {
        this.protocol = proto;
    }

    /**
     * Every void the protocol reads, nested arms included.
     *
     * @return The indices of the voids, ascending
     */
    SortedSet<Integer> all() {
        final SortedSet<Integer> out = this.direct();
        this.protocol.moves().stream()
            .flatMap(step -> step.branches().stream())
            .forEach(arm -> out.addAll(new Reads(arm).all()));
        return out;
    }

    /**
     * The voids the block of this protocol declares.
     *
     * @param above The indices of the voids the enclosing blocks declared
     * @return The indices of the voids, ascending
     */
    SortedSet<Integer> own(final Set<Integer> above) {
        final Map<Integer, Integer> count = new HashMap<>(0);
        final Stream<Protocol> arms = this.protocol.moves().stream()
            .flatMap(step -> step.branches().stream());
        arms.forEach(
            arm -> new Reads(arm).all().forEach(index -> count.merge(index, 1, Integer::sum))
        );
        final SortedSet<Integer> out = this.direct();
        for (final Map.Entry<Integer, Integer> entry : count.entrySet()) {
            if (entry.getValue() > 1) {
                out.add(entry.getKey());
            }
        }
        out.removeAll(above);
        return out;
    }

    private SortedSet<Integer> direct() {
        final SortedSet<Integer> out = new TreeSet<>();
        final Stream<String> keys = Stream.concat(
            this.protocol.moves().stream()
                .filter(step -> step.atom().charAt(0) != '.' && !step.atom().startsWith("Φ."))
                .flatMap(step -> step.keys().stream()),
            Stream.concat(
                Stream.of(this.protocol.answer(), this.protocol.reason()),
                this.protocol.again().stream()
            )
        );
        keys.filter(key -> key.startsWith("sym:v"))
            .forEach(key -> out.add(Integer.parseInt(key.substring(5))));
        return out;
    }
}
