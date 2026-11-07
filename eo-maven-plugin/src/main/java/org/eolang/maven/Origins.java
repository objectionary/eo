/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
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
 * outside the program may fill it with anything at all. A declared {@code ^}
 * is one exception: it holds the formation it is written in, which the
 * locators tell. A void of a copy Java makes is the other, since the atom
 * promises what goes there. An atom returns a type, such as
 * {@code Φ.posix.time-return(Φ.number,Φ.number,Φ.number)}, and Java puts the
 * i-th type in its brackets into the i-th void of the formation, counting the
 * voids other than {@code ρ} in the order they are written. So the type
 * travels with the copy along the reads: a bare {@code ξ} in the body of one
 * of its attributes stands for that copy, and a read of its i-th void copies
 * the i-th type. A place that reads any other void, or reaches the void
 * through what it reads, has no formation here. That includes a void of a
 * copy made in EO, even of the same formation, and a void the brackets leave
 * out. A datum, a termination, and a place whose reads run back into
 * themselves have no formation here either.</p>
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
     * The voids of each formation other than {@code ρ}, in the order they
     * are written, by the formation, which is the order the brackets of a
     * type fill them in.
     */
    private final Map<String, List<String>> voids;

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
     * @param holes The voids of each formation other than {@code ρ}, in order
     * @param dispatches The reads, by the place each lands at
     * @param refs The formation each head points at
     */
    Origins(
        final Map<String, Map<String, String>> formations,
        final Map<String, List<String>> holes,
        final Map<String, Map.Entry<String, String>> dispatches,
        final Map<String, String> refs
    ) {
        this.objects = formations;
        this.voids = holes;
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
        return this.formation(place, "Φ", new HashMap<>(0)).map(
            type -> new Annotation(type).head()
        );
    }

    private Optional<String> formation(
        final String place, final String host, final Map<String, Optional<String>> seen
    ) {
        return Origins.once(
            String.join(" ", place, host),
            seen,
            () -> {
                final Optional<String> found;
                if (this.objects.containsKey(place)) {
                    found = Optional.of(place);
                } else if (this.reads.containsKey(place)) {
                    final Map.Entry<String, String> read = this.reads.get(place);
                    found = this.formation(read.getKey(), host, seen).flatMap(
                        owner -> this.attribute(owner, read.getValue(), seen)
                    );
                } else if (new Annotation(host).head().equals(this.heads.get(place))) {
                    found = Optional.of(host);
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
                    new Annotation(owner).head(), Collections.emptyMap()
                );
                final Optional<String> member = this.member(owner, name);
                final Optional<String> found;
                if (member.isPresent()) {
                    found = this.target(member.get(), owner, seen);
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

    private Optional<String> member(final String owner, final String name) {
        final Annotation type = new Annotation(owner);
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

    private Optional<String> target(
        final String place, final String host, final Map<String, Optional<String>> seen
    ) {
        final Optional<String> found;
        if (place.isEmpty()) {
            found = Optional.empty();
        } else if (this.objects.containsKey(new Annotation(place).head())) {
            found = Optional.of(place);
        } else {
            found = this.formation(place, host, seen);
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
