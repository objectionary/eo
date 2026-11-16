/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
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
 * outside the program may fill it with anything at all. There are four
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
 * bound by name goes into the void it names. A void that every call of the
 * program fills with a formation written right there holds one of those
 * formations, when they all have the same voids in the same order: in
 * {@code bool}, the void {@code fork} is filled only by {@code true} and
 * {@code false}, with {@code [^pick left right]} and
 * {@code [^state left right]}. A read of such a void copies none of them in
 * particular, so it gives a blurred {@link Copy}, which tells where the
 * arguments of a call on it land, and nothing more (#9952). The program
 * stands here for every caller there is: a caller in Java, or in a program
 * built later, that fills the void with another formation breaks this. A
 * place that copies the formation without a call leaves the void empty, and
 * then the runtime puts the terminator there, which has no voids to name, so one
 * such place is enough to say nothing about the void.</p>
 *
 * <p>When the calls of the program say nothing of a void, the engine may
 * still know it, since it follows the heads of the calls through the callers
 * it saw, and the calls alone may not: a literal {@code number 42} puts the
 * copy {@code Φ.bytes(data)} into the {@code φ} of {@code number}, and a call
 * {@code number x}, with {@code x} a void, puts whatever its callers put into
 * {@code x}. When the engine answers that a void holds one formation and
 * nothing else, with no terminator, no datum and no unknown, the void holds
 * that formation here too (#9953). So {@code Φ.number.φ} holds
 * {@code Φ.bytes}, and {@code eq}, {@code lt} or {@code minus} of a number
 * are found there. This takes the program for every caller, as above, and
 * one more thing: that Java puts into the voids of the copies it makes what
 * the program puts there.</p>
 *
 * <p>Which calls copy a formation is found by following the head of every
 * call once, the first time a void is asked for. While that runs, no void is
 * filled, so a head that reads one stays unknown there.</p>
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
     * Each void other than {@code ρ}, by its place: the formation, and the
     * name.
     */
    private final Map<String, Map.Entry<String, String>> holes;

    /**
     * The calls that copy each formation, with nothing in its voids yet, by
     * the formation.
     */
    private final Map<String, List<String>> copies;

    /**
     * The formations that some place copies without a call, so that the
     * runtime puts the terminator into their voids.
     */
    private final Set<String> bare;

    /**
     * The one formation the engine saw each void hold, by the place of the
     * void, for the voids where it saw nothing else.
     */
    private final Map<String, String> answered;

    /**
     * What fills each void, by the place of the void, once it is known.
     */
    private final Map<String, Optional<Copy>> fills;

    /**
     * How far {@link #holes} and {@link #copies} are filled: zero before,
     * one while, and two after.
     */
    private final AtomicInteger stage;

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
        this(places, applications, dispatches, refs, Collections.emptyMap());
    }

    /**
     * Ctor.
     *
     * @param places The place each attribute of a formation leads to
     * @param applications What the calls put into the voids
     * @param dispatches The reads, by the place each lands at
     * @param refs The formation each head points at
     * @param sole The one formation the engine saw each void hold
     */
    Origins(
        final Map<String, Map<String, String>> places,
        final Calls applications,
        final Map<String, Map.Entry<String, String>> dispatches,
        final Map<String, String> refs,
        final Map<String, String> sole
    ) {
        this.attributes = places;
        this.calls = applications;
        this.reads = dispatches;
        this.heads = refs;
        this.answered = sole;
        this.holes = new HashMap<>(0);
        this.copies = new HashMap<>(0);
        this.bare = new HashSet<>(0);
        this.fills = new HashMap<>(0);
        this.stage = new AtomicInteger();
    }

    /**
     * The formation the place copies.
     *
     * @param place The locator of the place
     * @return The locator of the formation, or nothing when the code alone
     *  does not fix it
     */
    Optional<String> formation(final String place) {
        return this.formation(place, Origins.root(), new HashMap<>(0)).map(
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
                    found = this.head(place, host, seen).or(() -> this.filled(place));
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
            () -> {
                final Optional<Copy> found;
                if (owner.sole()) {
                    found = owner.member(name, this.calls).map(this::copy).orElseGet(
                        () -> this.argument(owner, name, seen).or(
                            () -> this.binding(owner, name, seen)
                        )
                    );
                } else {
                    found = this.argument(owner, name, seen);
                }
                return found;
            }
        );
    }

    private Optional<Copy> argument(
        final Copy owner, final String name, final Map<Map.Entry<?, ?>, Optional<Copy>> seen
    ) {
        return owner.argument(name, this.calls).flatMap(
            arg -> this.formation(arg.getKey(), arg.getValue(), seen)
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
            found = this.attribute(owner, "φ", seen).flatMap(
                inner -> this.attribute(inner, name, seen)
            );
        }
        return found;
    }

    private Optional<Copy> filled(final String place) {
        final Optional<Copy> found;
        if (this.fills.containsKey(place)) {
            found = this.fills.get(place);
        } else if (this.stage.compareAndSet(0, 1)) {
            this.index();
            this.stage.set(2);
            found = this.filled(place);
        } else if (this.stage.get() == 1) {
            found = Optional.empty();
        } else {
            found = Optional.ofNullable(this.holes.get(place)).flatMap(
                hole -> this.shape(hole).or(() -> this.answer(place))
            );
            this.fills.put(place, found);
        }
        return found;
    }

    private void index() {
        this.voids();
        this.empties();
        for (final String call : this.calls.places()) {
            this.fresh(call).ifPresent(
                head -> this.copies.computeIfAbsent(
                    head, key -> new ArrayList<>(1)
                ).add(call)
            );
        }
    }

    private void voids() {
        for (final Map.Entry<String, Map<String, String>> form : this.attributes.entrySet()) {
            for (final String name : this.calls.voids(form.getKey())) {
                final String place = form.getValue().get(name);
                if (place != null) {
                    this.holes.put(place, Map.entry(form.getKey(), name));
                }
            }
        }
    }

    private void empties() {
        final Set<String> owners = new HashSet<>(0);
        for (final Map.Entry<String, String> read : this.reads.values()) {
            owners.add(read.getKey());
        }
        final Set<String> values = new TreeSet<>(this.reads.keySet());
        for (final String head : this.heads.keySet()) {
            if (!owners.contains(head)) {
                values.add(head);
            }
        }
        for (final String place : values) {
            if (!this.calls.contains(place)
                && !"ρ".equals(this.reads.getOrDefault(place, Map.entry("", "")).getValue())) {
                this.fresh(place).ifPresent(this.bare::add);
            }
        }
    }

    private Optional<String> fresh(final String place) {
        return this.head(place, Origins.root(), new HashMap<>(0)).filter(
            head -> head.equals(new Copy(new Annotation(head.formation())))
        ).map(Copy::formation);
    }

    private Optional<Copy> shape(final Map.Entry<String, String> hole) {
        final Set<String> fillers = new TreeSet<>();
        boolean closed = true;
        for (final String call : this.copies.getOrDefault(hole.getKey(), List.of())) {
            final Optional<String> arg = this.calls.argument(
                call, hole.getKey(), hole.getValue()
            );
            if (arg.isPresent() && this.attributes.containsKey(arg.get())) {
                fillers.add(arg.get());
            } else {
                closed = false;
            }
        }
        final Optional<Copy> found;
        if (closed && !fillers.isEmpty() && !this.bare.contains(hole.getKey())
            && fillers.stream().map(this.calls::voids).distinct().count() == 1L) {
            found = Optional.of(
                new Copy(new Annotation(fillers.iterator().next()), fillers.size() == 1)
            );
        } else {
            found = Optional.empty();
        }
        return found;
    }

    private Optional<Copy> answer(final String place) {
        return Optional.ofNullable(this.answered.get(place)).filter(
            this.attributes::containsKey
        ).map(form -> new Copy(new Annotation(form)));
    }

    private static Copy root() {
        return new Copy(new Annotation("Φ"));
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
