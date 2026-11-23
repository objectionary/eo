/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven.dealpha;

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
import java.util.stream.Collectors;
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
 * bound by name goes into the void it names. A void holds one of the
 * formations that the calls of the program put there, when every argument
 * they put there copies a formation the code fixes: in {@code bool}, the void
 * {@code fork} is filled only by {@code true} and {@code false}, with
 * {@code [^pick left right]} and {@code [^state left right]}. An argument may
 * be a formation written right there, a read off what another call makes, or
 * a void of the caller, which is looked up the same way, one hop at a time
 * (#9954). A read of a void that several formations fill copies none of them
 * in particular, so it gives a blurred {@link Copy}, which tells where the
 * arguments of a call on it land when all those formations have the same
 * voids in the same order (#9952). When they do not, as with
 * {@code Φ.number} and {@code Φ.number.plus} filling the void {@code depth}
 * of a recursive helper, a call on the void says nothing, since its
 * arguments land in different voids of different formations, while a name
 * read off it, such as {@code depth.gt}, is still found (#9978). A name read
 * off a blurred copy,
 * that is not an argument of its call, is looked up in each of those
 * formations, as if the same call had made a copy of it. It copies a
 * formation only when what they give agrees, as {@link Copy#agreed} says:
 * the {@code φ} of {@code [^state left right]} is {@code right}, the
 * {@code φ} of {@code [^pick left right]} is {@code left}, and {@code .if}
 * read off what {@code 5.eq 8} returns is found when both {@code left} and
 * {@code right} of that call lead to {@code bool} (#9973). An atom that
 * returns a copy of the formation with brackets, such as
 * {@code Φ.posix.time-return(Φ.number,Φ.number,Φ.number)}, fills the void
 * too, with the type its brackets give, and when they give none for the
 * void, nothing is said about it. An atom that returns a type with no
 * brackets, such as {@code Φ.bool}, builds its copy out of the objects of
 * the program, as {@code Data.ToPhi} makes {@code Φ.true}, so it fills no
 * void itself.
 * A generic atom, such as {@code [] > recovered /A} with
 * {@code ? > value /A?} and {@code ? > alternative /A}, returns whatever
 * was put into one of its voids that carry its type variable, and the code
 * does not say which one. So its {@code φ} copies a formation only when
 * every such void holds a copy of that same formation: {@code recovered 42 7}
 * copies {@code Φ.number}, while in {@code recovered x 7} with {@code x} a
 * {@code Φ.string}, nothing is said about it (#9955).
 * The program stands here for every caller there is: a caller in Java, or in
 * a program built later, that fills the void with another formation breaks
 * this. A place that copies the formation without a call leaves the void
 * empty, and then the runtime puts the terminator there, which has no voids
 * to name, so one such place is enough to say nothing about the void.</p>
 *
 * <p>A helper may pass its own void back to itself, as in
 * {@code rec (n.minus 1)}, so the arguments of a void may lead back to it.
 * Such a void is first taken to hold nothing, and an argument that leads back
 * to it adds nothing to what it holds. Then it is taken to hold what the
 * other arguments put there, and every argument is looked up again, until
 * what it holds stops changing. A recursive call passes on only what some
 * outside call put there first, so a void that no outside call fills holds
 * nothing the code fixes. What a void holds is remembered only when its
 * lookup did not lean on another void still being looked up.</p>
 *
 * <p>When the calls of the program say nothing of a void {@code φ}, the
 * engine may still know it, since it follows the heads of the calls through
 * the callers it saw, and the calls alone may not: a literal
 * {@code number 42} puts the copy {@code Φ.bytes(data)} into the {@code φ}
 * of {@code number}, and a call {@code number x}, with {@code x} a void,
 * puts whatever its callers put into {@code x}. When the engine answers that
 * a void {@code φ} holds one formation and nothing else, with no terminator,
 * no datum and no unknown, the void holds that formation here too (#9953).
 * So {@code Φ.number.φ} holds {@code Φ.bytes}, and {@code eq}, {@code lt} or
 * {@code minus} of a number are found there. This assumes that Java puts
 * into the {@code φ} of a copy it makes what the program puts there, as
 * {@code Data.ToPhi} puts bytes into a {@code number} or a {@code string}.
 * No other void is read this way: a caller in Java fills a void such as
 * {@code scope} of {@code socket.connect} with any object it likes, so the
 * callers the engine saw say nothing about it.</p>
 *
 * <p>A terminator, such as {@code T "it broke"}, is where the program stops,
 * so nothing is ever read off what it makes, and no call on it ever runs.
 * Its place copies no formation, yet it is not unknown either: a read off it,
 * or a call on it, is a terminator too, and when the candidates of a blurred
 * copy are agreed on, a terminator among them is left out. So in
 * {@code x.if (T "too big") 42}, a name read off the {@code .if} is looked up
 * in {@code 42} alone (#9982).</p>
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
     * The places of the terminators, where the program stops.
     */
    private final Set<String> stops;

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
     * What the calls and the atoms put into each void.
     */
    private final Fillers fillers;

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
        this(places, applications, dispatches, refs, sole, Collections.emptySet());
    }

    /**
     * Ctor.
     *
     * @param places The place each attribute of a formation leads to
     * @param applications What the calls put into the voids
     * @param dispatches The reads, by the place each lands at
     * @param refs The formation each head points at
     * @param sole The one formation the engine saw each void hold
     * @param ends The places of the terminators
     */
    Origins(
        final Map<String, Map<String, String>> places,
        final Calls applications,
        final Map<String, Map.Entry<String, String>> dispatches,
        final Map<String, String> refs,
        final Map<String, String> sole,
        final Set<String> ends
    ) {
        this.attributes = places;
        this.stops = ends;
        this.calls = applications;
        this.reads = dispatches;
        this.heads = refs;
        this.answered = sole;
        this.holes = new HashMap<>(0);
        this.copies = new HashMap<>(0);
        this.bare = new HashSet<>(0);
        this.fills = new HashMap<>(0);
        this.stage = new AtomicInteger();
        this.fillers = new Fillers(
            places, applications, this.copies, this.bare,
            (place, seen) -> this.formation(place, Origins.root(), seen)
        );
    }

    /**
     * The formation the place copies.
     *
     * @param place The locator of the place
     * @return The locator of the formation, or nothing when the code alone
     *  does not fix it
     */
    Optional<String> formation(final String place) {
        return this.formation(place, Origins.root(), this.fillers.lookups()).filter(
            copy -> !copy.stops() && !copy.loops()
        ).map(Copy::formation);
    }

    private Optional<Copy> formation(
        final String place, final Copy host, final Lookups seen
    ) {
        return seen.once(
            Map.entry(place, host),
            () -> {
                final Optional<Copy> found;
                if (this.stops.contains(place)) {
                    found = Optional.of(Copy.terminator());
                } else if (this.attributes.containsKey(place)) {
                    found = Optional.of(new Copy(new Annotation(place)));
                } else if (this.calls.contains(place)) {
                    found = this.head(place, host, seen)
                        .filter(head -> head.stops() || head.uniform(this.calls))
                        .map(head -> Origins.applied(head, place, host));
                } else {
                    found = this.head(place, host, seen).or(() -> this.filled(place));
                }
                return found;
            }
        );
    }

    private Optional<Copy> head(
        final String place, final Copy host, final Lookups seen
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
        final Copy owner, final String name, final Lookups seen
    ) {
        final Optional<Copy> read;
        if (owner.stops()) {
            read = Optional.of(owner);
        } else {
            read = this.member(owner, name, seen);
        }
        return read;
    }

    private Optional<Copy> member(
        final Copy owner, final String name, final Lookups seen
    ) {
        return seen.once(
            Map.entry(owner, name),
            () -> {
                final Optional<Copy> found;
                if (owner.sole()) {
                    found = owner.member(name, this.calls).map(this::copy).orElseGet(
                        () -> this.argument(owner, name, seen).or(
                            () -> this.binding(owner, name, seen)
                        )
                    );
                } else {
                    found = this.argument(owner, name, seen).or(
                        () -> Copy.agreed(
                            owner.candidates().stream().map(
                                each -> this.attribute(each, name, seen)
                            ).collect(Collectors.toList()),
                            this.calls
                        )
                    );
                }
                return found;
            }
        );
    }

    private Optional<Copy> argument(
        final Copy owner, final String name, final Lookups seen
    ) {
        return owner.argument(name, this.calls).flatMap(
            arg -> this.formation(arg.getKey(), arg.getValue(), seen)
        );
    }

    private Optional<Copy> binding(
        final Copy owner, final String name, final Lookups seen
    ) {
        final Map<String, String> places = this.attributes.getOrDefault(
            owner.formation(), Collections.emptyMap()
        );
        final Optional<Copy> found;
        if (places.containsKey(name)) {
            found = this.formation(places.get(name), owner, seen);
        } else if ("φ".equals(name) && this.calls.slots(owner.formation()).isEmpty()) {
            found = this.calls.returned(owner.formation()).flatMap(this::copy);
        } else if ("φ".equals(name)) {
            found = this.generic(owner, places, seen);
        } else {
            found = this.attribute(owner, "φ", seen).flatMap(
                inner -> this.attribute(inner, name, seen)
            );
        }
        return found;
    }

    private Optional<Copy> generic(
        final Copy owner, final Map<String, String> places,
        final Lookups seen
    ) {
        final List<Optional<Copy>> held = new ArrayList<>(2);
        for (final String slot : this.calls.slots(owner.formation())) {
            held.add(
                places.entrySet().stream().filter(
                    entry -> entry.getValue().equals(slot)
                ).map(Map.Entry::getKey).findFirst().flatMap(
                    hole -> this.attribute(owner, hole, seen)
                )
            );
        }
        return Copy.agreed(held, this.calls);
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
        } else if (this.fillers.open(place)) {
            found = this.fillers.hit(place);
        } else {
            found = this.settled(place);
        }
        return found;
    }

    private Optional<Copy> settled(final String place) {
        final Map.Entry<String, String> hole = this.holes.get(place);
        final Optional<Copy> found;
        if (hole == null) {
            found = Optional.empty();
            this.fills.put(place, found);
        } else {
            final Map.Entry<Optional<Copy>, Boolean> run = this.fillers.held(place, hole);
            found = run.getKey().or(() -> this.answer(hole, place));
            if (run.getValue()) {
                this.fills.put(place, found);
            }
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
        return this.head(place, Origins.root(), this.fillers.lookups()).filter(
            head -> head.equals(new Copy(new Annotation(head.formation())))
        ).map(Copy::formation);
    }

    private Optional<Copy> answer(final Map.Entry<String, String> hole, final String place) {
        return Optional.ofNullable(this.answered.get(place)).filter(
            form -> "φ".equals(hole.getValue()) && this.attributes.containsKey(form)
        ).map(form -> new Copy(new Annotation(form)));
    }

    private static Copy applied(final Copy head, final String place, final Copy host) {
        final Copy made;
        if (head.stops()) {
            made = head;
        } else {
            made = head.applied(place, host);
        }
        return made;
    }

    private static Copy root() {
        return new Copy(new Annotation("Φ"));
    }

    private Optional<Copy> copy(final Annotation type) {
        return Optional.of(type).filter(
            kind -> this.attributes.containsKey(kind.head())
        ).map(Copy::new);
    }
}
