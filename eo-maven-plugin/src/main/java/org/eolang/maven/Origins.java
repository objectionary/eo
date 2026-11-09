/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;
import org.eolang.inference.Annotation;

/**
 * The formation a place of the program copies, when the code alone says
 * which.
 *
 * <p>A place is a formation itself, a head, or a read of a name off another
 * place. A head is a bare {@code Φ} or a bare {@code ξ}, and the text alone
 * tells which formation it is. A read follows its name into the formation the
 * other place copies: a bound attribute copies what its body copies, and a
 * name the formation does not bind is looked up behind its {@code φ}, or
 * behind what an atom says it returns, since an atom decorates that.</p>
 *
 * <p>A void says nothing, since whoever makes the copy fills it, and a caller
 * outside the program may fill it with anything at all. There are three
 * exceptions. A declared {@code ^} holds the formation it is written in,
 * which the locators tell. A void of a copy Java makes holds what the atom
 * promises: an atom returns a type, such as
 * {@code Φ.posix.time-return(Φ.number,Φ.number,Φ.number)}, and Java puts the
 * i-th type in its brackets into the i-th void of the formation, counting the
 * voids other than {@code ρ} in the order they are written. A void of a copy
 * that a call at one place of the program makes holds the argument written
 * there, since no other caller can change it: in
 * {@code posix.stat-return code message mode size}, the void {@code cde}
 * holds what {@code code} copies, read in the copy the call is written in. A
 * positional argument goes into the void at its position, and an argument
 * bound by name goes into the void it names.</p>
 *
 * <p>So a {@link Copy} carries what fills its voids along the reads: a bare
 * {@code ξ} in the body of one of its attributes stands for that copy, and a
 * read of one of its voids copies what fills it. A place that reads any other
 * void, or reaches the void through what it reads, has no formation here.
 * That includes a void the brackets leave out, a void of a copy whose call
 * passes fewer or more arguments than the formation has voids, and an
 * argument that itself reads such a void. A datum, a termination, a type
 * whose head is no formation of the program, and a place whose reads run back
 * into themselves have no formation here either.</p>
 *
 * @since 0.76.0
 */
final class Origins {

    /**
     * The place each attribute of a formation leads to, by the name, by the
     * formation: the body of a bound attribute, the formation a declared
     * {@code ρ} is written in, or the void itself for any other void. Nothing
     * is written at a void, so its place leads to no formation.
     */
    private final Map<String, Map<String, String>> attributes;

    /**
     * What the calls of the program put into the voids of the copies they
     * make, and what the atoms return.
     */
    private final Calls calls;

    /**
     * The reads, by the place each lands at: the place the name is read off,
     * and the name.
     */
    private final Map<String, Map.Entry<String, String>> reads;

    /**
     * The formation each head points at, by the place of the head.
     */
    private final Map<String, String> heads;

    /**
     * Ctor.
     *
     * @param places The place each attribute of a formation leads to
     * @param applications What the calls put into the voids
     * @param dispatches The reads, by the place each lands at
     * @param refs The formation each head points at
     */
    Origins(
        final Map<String, Map<String, String>> places,
        final Calls applications,
        final Map<String, Map.Entry<String, String>> dispatches,
        final Map<String, String> refs
    ) {
        this.attributes = places;
        this.calls = applications;
        this.reads = dispatches;
        this.heads = refs;
    }

    /**
     * The formation the place copies.
     *
     * @param place The locator of the place
     * @return The locator of the formation, or nothing when the code alone
     *  does not fix it
     */
    Optional<String> formation(final String place) {
        return this.formation(place, new Copy(new Annotation("Φ")), new HashMap<>(0)).map(
            Copy::formation
        );
    }

    private Optional<Copy> formation(
        final String place, final Copy host, final Map<Map.Entry<?, ?>, Optional<Copy>> seen
    ) {
        return Origins.once(
            Map.entry(place, host),
            seen,
            () -> {
                final Optional<Copy> found;
                if (this.attributes.containsKey(place)) {
                    found = Optional.of(new Copy(new Annotation(place)));
                } else if (this.calls.contains(place)) {
                    found = this.head(place, host, seen).map(head -> head.applied(place, host));
                } else {
                    found = this.head(place, host, seen);
                }
                return found;
            }
        );
    }

    private Optional<Copy> head(
        final String place, final Copy host, final Map<Map.Entry<?, ?>, Optional<Copy>> seen
    ) {
        final Optional<Copy> found;
        if (this.reads.containsKey(place)) {
            final Map.Entry<String, String> read = this.reads.get(place);
            found = this.formation(read.getKey(), host, seen).flatMap(
                owner -> this.attribute(owner, read.getValue(), seen)
            );
        } else if (host.formation().equals(this.heads.get(place))) {
            found = Optional.of(host);
        } else {
            found = Optional.ofNullable(this.heads.get(place)).map(
                ref -> new Copy(new Annotation(ref))
            );
        }
        return found;
    }

    private Optional<Copy> attribute(
        final Copy owner, final String name, final Map<Map.Entry<?, ?>, Optional<Copy>> seen
    ) {
        return Origins.once(
            Map.entry(owner, name),
            seen,
            () -> owner.member(name, this.calls).map(this::copy).orElseGet(
                () -> owner.argument(name, this.calls).map(
                    arg -> this.formation(arg.getKey(), arg.getValue(), seen)
                ).orElseGet(() -> this.binding(owner, name, seen))
            )
        );
    }

    private Optional<Copy> binding(
        final Copy owner, final String name, final Map<Map.Entry<?, ?>, Optional<Copy>> seen
    ) {
        final Map<String, String> places = this.attributes.getOrDefault(
            owner.formation(), Collections.emptyMap()
        );
        final Optional<Copy> found;
        if (places.containsKey(name)) {
            found = this.formation(places.get(name), owner, seen);
        } else if ("φ".equals(name)) {
            found = this.calls.returned(owner.formation()).flatMap(this::copy);
        } else {
            found = this.binding(owner, "φ", seen).flatMap(
                inner -> this.attribute(inner, name, seen)
            );
        }
        return found;
    }

    private Optional<Copy> copy(final Annotation type) {
        return Optional.of(type).filter(
            kind -> this.attributes.containsKey(kind.head())
        ).map(Copy::new);
    }

    private static Optional<Copy> once(
        final Map.Entry<?, ?> key,
        final Map<Map.Entry<?, ?>, Optional<Copy>> seen,
        final Supplier<Optional<Copy>> fresh
    ) {
        if (!seen.containsKey(key)) {
            seen.put(key, Optional.empty());
            seen.replace(key, fresh.get());
        }
        return seen.get(key);
    }
}
