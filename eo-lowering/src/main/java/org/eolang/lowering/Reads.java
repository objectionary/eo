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
 * The voids one protocol reads, and which of them its own block declares.
 *
 * <p>A void is dataized at the top of the innermost block that reaches
 * every use of it, so that an argument only one arm of a fork touches is
 * never forced when the other arm is taken. The block of a protocol
 * therefore declares the voids a step of its own reads or its answer
 * names, and the voids two or more arms of its forks read, since no
 * single arm dominates such a void; a void exactly one arm reads is
 * left to the protocol of that arm, which decides the same way. Whatever
 * an enclosing block declared already is in scope and is not declared
 * again.</p>
 *
 * @since 0.76.0
 */
public final class Reads {

    /**
     * The protocol.
     */
    private final Protocol protocol;

    /**
     * Ctor.
     * @param proto The protocol
     */
    public Reads(final Protocol proto) {
        this.protocol = proto;
    }

    /**
     * Every void the protocol reads, nested arms included.
     * @return The indices of the voids, ascending
     */
    public SortedSet<Integer> all() {
        final SortedSet<Integer> out = this.direct();
        this.protocol.moves().stream()
            .flatMap(step -> step.branches().stream())
            .forEach(arm -> out.addAll(new Reads(arm).all()));
        return out;
    }

    /**
     * The voids the block of this protocol declares.
     * @param above The indices of the voids the enclosing blocks declared
     * @return The indices of the voids, ascending
     */
    public SortedSet<Integer> own(final Set<Integer> above) {
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
            this.protocol.moves().stream().flatMap(step -> step.keys().stream()),
            Stream.of(this.protocol.answer())
        );
        keys.filter(key -> key.startsWith("sym:v"))
            .forEach(key -> out.add(Integer.parseInt(key.substring(5))));
        return out;
    }
}
