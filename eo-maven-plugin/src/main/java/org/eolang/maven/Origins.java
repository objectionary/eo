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
 * argument that itself reads such a void. A datum, a termination, and a place
 * whose reads run back into themselves have no formation here either.</p>
 *
 * @since 0.76.0
 */
final class Origins {

    /**
     * What each formation answers to a name, by the name: the place of a
     * bound attribute, the formation of a declared {@code ρ}, the type an
     * atom with no {@code φ} says it returns, brackets and all, under
     * {@code φ}, and an empty string for any other void.
     */
    private final Map<String, Map<String, String>> objects;

    /**
     * What the calls of the program put into the voids of the copies they
     * make.
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
     * @param formations What each formation answers to a name
     * @param applications What the calls put into the voids
     * @param dispatches The reads, by the place each lands at
     * @param refs The formation each head points at
     */
    Origins(
        final Map<String, Map<String, String>> formations,
        final Calls applications,
        final Map<String, Map.Entry<String, String>> dispatches,
        final Map<String, String> refs
    ) {
        this.objects = formations;
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
        return this.formation(place, new Copy("Φ"), new HashMap<>(0)).map(Copy::formation);
    }

    private Optional<Copy> formation(
        final String place, final Copy host, final Map<String, Optional<Copy>> seen
    ) {
        return Origins.once(
            String.join(" ", place, host.toString()),
            seen,
            () -> {
                final Optional<Copy> found;
                if (this.objects.containsKey(place)) {
                    found = Optional.of(new Copy(place));
                } else {
                    found = this.head(place, host, seen).map(
                        head -> this.calls.copy(place, head, host)
                    );
                }
                return found;
            }
        );
    }

    private Optional<Copy> head(
        final String place, final Copy host, final Map<String, Optional<Copy>> seen
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
            found = Optional.ofNullable(this.heads.get(place)).map(Copy::new);
        }
        return found;
    }

    private Optional<Copy> attribute(
        final Copy owner, final String name, final Map<String, Optional<Copy>> seen
    ) {
        return Origins.once(
            String.join(" ", owner.toString(), name),
            seen,
            () -> {
                final Map<String, String> attrs = this.objects.getOrDefault(
                    owner.formation(), Collections.emptyMap()
                );
                final Optional<Map.Entry<String, Copy>> filler = owner.filler(name, this.calls);
                final Optional<Copy> found;
                if (filler.isPresent()) {
                    found = this.target(filler.get().getKey(), filler.get().getValue(), seen);
                } else if (attrs.containsKey(name)) {
                    found = this.target(attrs.get(name), owner, seen);
                } else if (attrs.containsKey("φ")) {
                    found = this.target(attrs.get("φ"), owner, seen).flatMap(
                        inner -> this.attribute(inner, name, seen)
                    );
                } else {
                    found = Optional.empty();
                }
                return found;
            }
        );
    }

    private Optional<Copy> target(
        final String place, final Copy host, final Map<String, Optional<Copy>> seen
    ) {
        final Optional<Copy> found;
        if (place.isEmpty()) {
            found = Optional.empty();
        } else if (this.objects.containsKey(new Annotation(place).head())) {
            found = Optional.of(new Copy(place));
        } else {
            found = this.formation(place, host, seen);
        }
        return found;
    }

    private static Optional<Copy> once(
        final String key,
        final Map<String, Optional<Copy>> seen,
        final Supplier<Optional<Copy>> fresh
    ) {
        if (!seen.containsKey(key)) {
            seen.put(key, Optional.empty());
            seen.replace(key, fresh.get());
        }
        return seen.get(key);
    }
}
