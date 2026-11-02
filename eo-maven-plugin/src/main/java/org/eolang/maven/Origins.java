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
 * outside the program may fill it with anything at all. The one void that
 * says something is {@code ρ}: a declared {@code ^} holds the formation it is
 * written in, which the locators tell. So a place that reads any other void,
 * or reaches the void through what it reads, has no formation here, and
 * neither has a datum, a termination, or a place whose reads run back into
 * themselves.</p>
 *
 * @since 0.76.0
 */
final class Origins {

    /**
     * What each formation answers to a name, by the name: the place of a
     * bound attribute, the formation of a declared {@code ρ}, what an atom
     * with no {@code φ} says it returns under {@code φ}, and an empty string
     * for any other void.
     */
    private final Map<String, Map<String, String>> objects;

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
     * @param dispatches The reads, by the place each lands at
     * @param refs The formation each head points at
     */
    Origins(
        final Map<String, Map<String, String>> formations,
        final Map<String, Map.Entry<String, String>> dispatches,
        final Map<String, String> refs
    ) {
        this.objects = formations;
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
        return this.formation(place, new HashMap<>(0));
    }

    private Optional<String> formation(
        final String place, final Map<String, Optional<String>> seen
    ) {
        return Origins.once(
            place,
            seen,
            () -> {
                final Optional<String> found;
                if (this.objects.containsKey(place)) {
                    found = Optional.of(place);
                } else if (this.reads.containsKey(place)) {
                    final Map.Entry<String, String> read = this.reads.get(place);
                    found = this.formation(read.getKey(), seen).flatMap(
                        owner -> this.attribute(owner, read.getValue(), seen)
                    );
                } else {
                    found = Optional.ofNullable(this.heads.get(place));
                }
                return found;
            }
        );
    }

    private Optional<String> attribute(
        final String owner, final String name, final Map<String, Optional<String>> seen
    ) {
        return Origins.once(
            String.join(" ", owner, name),
            seen,
            () -> {
                final Map<String, String> attrs = this.objects.getOrDefault(
                    owner, Collections.emptyMap()
                );
                final Optional<String> found;
                if (attrs.containsKey(name)) {
                    found = this.target(attrs.get(name), seen);
                } else if (attrs.containsKey("φ")) {
                    found = this.target(attrs.get("φ"), seen).flatMap(
                        inner -> this.attribute(inner, name, seen)
                    );
                } else {
                    found = Optional.empty();
                }
                return found;
            }
        );
    }

    private Optional<String> target(final String place, final Map<String, Optional<String>> seen) {
        final Optional<String> found;
        if (place.isEmpty()) {
            found = Optional.empty();
        } else {
            found = this.formation(place, seen);
        }
        return found;
    }

    private static Optional<String> once(
        final String key,
        final Map<String, Optional<String>> seen,
        final Supplier<Optional<String>> fresh
    ) {
        if (!seen.containsKey(key)) {
            seen.put(key, Optional.empty());
            seen.replace(key, fresh.get());
        }
        return seen.get(key);
    }
}
