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
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import org.eolang.inference.Annotation;

/**
 * What the calls of the program and the atoms put into a void, when the
 * code alone says that.
 *
 * <p>Every call that copies the formation puts an argument into the void,
 * and every atom that returns a copy of it with brackets puts there the type
 * its brackets give. The void holds one of the formations these copy, when
 * each of them copies a formation the code fixes, and no place copies the
 * formation without a call. With one formation the copy is sharp, and with
 * several it is blurred. Those formations do not have to have the same voids:
 * a name read off the void is looked up in each of them, while a call on the
 * void is refused when their voids differ, as {@link Copy#uniform} says
 * (#9978).</p>
 *
 * <p>A call that puts a terminator into the void adds nothing to what the
 * void holds, since the program stops before the copy is made (#9982).
 * Neither does an argument that is the very lookup still running, as
 * {@link Copy#looped()} says (#9983).</p>
 *
 * <p>A place that copies the formation without a call, or a call that leaves
 * the void out, as {@link Calls#leaves} says, leaves the void empty. The code
 * does not say what is there then, since an atom, or a helper the formation
 * is handed to, may fill it, so such a place makes the void unknown. There is
 * one exception: some call of the program puts a terminator into the void,
 * such as {@code safe 5 T} in a test of {@code [x cant] > safe}. That call
 * shows that the void may be left with the terminator in it, so a place that
 * leaves it empty adds nothing more. Then the void holds the terminator, when
 * no other call fills it, and what the other calls put there otherwise,
 * in a copy that may be the terminator instead, as {@link Copy#ends()} says,
 * since a call on the void may then land in the terminator (#9997). A void
 * that would be known this way, if only some call put a terminator into it,
 * is remembered, as {@link #vacant()} says, so that the
 * test it lacks may be named.</p>
 *
 * <p>An argument may lead back to the void itself, when a helper passes its
 * own void to itself. In the first round the void is taken to hold nothing,
 * and such an argument adds nothing. In each next round the void is taken to
 * hold what the round before found, and every argument is looked up again,
 * until what the void holds stops changing.</p>
 *
 * @since 0.76.0
 */
final class Fillers {

    /**
     * The place each attribute of a formation leads to, by the name, by the
     * formation.
     */
    private final Map<String, Map<String, String>> attributes;

    /**
     * What the calls of the program put into the voids of the copies they
     * make, and what the atoms return.
     */
    private final Calls calls;

    /**
     * The calls that copy each formation, by the formation.
     */
    private final Map<String, List<String>> copies;

    /**
     * The formations that some place copies without a call.
     */
    private final Set<String> bare;

    /**
     * The voids whose fillers are being looked at right now.
     */
    private final Guesses guesses;

    /**
     * The copy a place makes, looked up with the lookups of one round.
     */
    private final BiFunction<String, Lookups, Optional<Copy>> lookup;

    /**
     * The voids that are left empty and would be known if some call put a
     * terminator into them: the formation and the name of each.
     */
    private final Set<Map.Entry<String, String>> empties;

    /**
     * What each void was found to hold while it leaned on other open voids,
     * each with the situation it was found in, by the place of the void.
     */
    private final Map<String, List<Map.Entry<Situation, Optional<Copy>>>> kept;

    /**
     * The places of the voids found to hold what they hold for good.
     */
    private final Set<String> settled;

    /**
     * Ctor.
     *
     * @param places The place each attribute of a formation leads to
     * @param applications What the calls put into the voids
     * @param made The calls that copy each formation
     * @param empty The formations that some place copies without a call
     * @param copy The copy a place makes
     */
    Fillers(
        final Map<String, Map<String, String>> places,
        final Calls applications,
        final Map<String, List<String>> made,
        final Set<String> empty,
        final BiFunction<String, Lookups, Optional<Copy>> copy
    ) {
        this.attributes = places;
        this.calls = applications;
        this.copies = made;
        this.bare = empty;
        this.guesses = new Guesses();
        this.lookup = copy;
        this.empties = new HashSet<>(0);
        this.kept = new HashMap<>(0);
        this.settled = new HashSet<>(0);
    }

    /**
     * The voids that the places of the program leave empty, and that would be
     * known if some call put a terminator into them, among the voids looked
     * at so far.
     *
     * <p>Such a void needs a test that passes the terminator to it, when it
     * may be left empty indeed: then the program itself says that it may hold
     * the terminator. A void that an atom, or a helper, fills is listed too,
     * since the code does not tell that from a void left empty (#9997).</p>
     *
     * @return The formation and the name of each void
     */
    Set<Map.Entry<String, String>> vacant() {
        return this.empties;
    }

    /**
     * Whether the fillers of the void are being looked at right now.
     *
     * @param place The place of the void
     * @return TRUE if they are
     */
    boolean open(final String place) {
        return this.guesses.open(place);
    }

    /**
     * What a void whose fillers are being looked at is taken to hold
     * meanwhile.
     *
     * @param place The place of the void
     * @return The guess, or nothing in the first round
     */
    Optional<Copy> hit(final String place) {
        return this.guesses.hit(place);
    }

    /**
     * A fresh cache of lookups, aware of the voids whose fillers are being
     * looked at.
     *
     * @return The lookups
     */
    Lookups lookups() {
        return new Lookups(this.guesses);
    }

    /**
     * What a void holds, once what it is taken to hold stops changing.
     *
     * <p>It takes at most eight rounds, and after that the void is said to
     * hold nothing the code fixes.</p>
     *
     * <p>What the void holds may lean on the guess of another void that is
     * still open. Then it can't be remembered for good, but it is kept
     * together with the situation it was found in, as {@link Situation} says,
     * and given back when asked for again in the same situation, instead of
     * being worked out again (#10020). The situation is the same when three
     * things are the same as they were:</p>
     *
     * <ul>
     *   <li>every open void it leaned on is still open, with the same
     *   guess;</li>
     *   <li>no void it opened itself is open now, since then the work would
     *   take the guess of that void instead of working it out;</li>
     *   <li>no void it opened itself is known for good now, since then the
     *   work would take what it holds for good instead of working it
     *   out.</li>
     * </ul>
     *
     * <p>Nothing else is read by the work, since what is known for good never
     * changes, so in the same situation it finds the same copy. The voids
     * it leaned on and opened are noted again, so that the lookup in progress
     * still knows it leans on them.</p>
     *
     * @param place The place of the void
     * @param hole The formation of the void, and its name
     * @return The copy, or nothing when the code does not fix it, and whether
     *  it may be remembered, which it may not when it leans on what another
     *  void, whose fillers are still being looked at, is taken to hold
     */
    Map.Entry<Optional<Copy>, Boolean> held(
        final String place, final Map.Entry<String, String> hole
    ) {
        final Optional<Map.Entry<Situation, Optional<Copy>>> again = this.kept.getOrDefault(
            place, List.of()
        ).stream().filter(
            each -> this.guesses.holds(each.getKey())
                && Collections.disjoint(each.getKey().opened(), this.settled)
        ).findFirst();
        final Map.Entry<Optional<Copy>, Boolean> found;
        if (again.isPresent()) {
            this.guesses.replay(again.get().getKey());
            found = Map.entry(again.get().getValue(), false);
        } else {
            found = this.fresh(place, hole);
        }
        return found;
    }

    private Map.Entry<Optional<Copy>, Boolean> fresh(
        final String place, final Map.Entry<String, String> hole
    ) {
        final List<Boolean> lacking = new ArrayList<>(1);
        final Map.Entry<Optional<Copy>, Situation> run = this.guesses.situated(
            () -> {
                this.guesses.guess(place, Optional.empty());
                final Optional<Copy> made = this.held(
                    place, hole, Optional.empty(), 8, lacking
                );
                this.guesses.close(place);
                return made;
            }
        );
        final boolean done = run.getValue().leaned().isEmpty();
        if (done) {
            this.settled.add(place);
            if (lacking.contains(true)) {
                this.empties.add(hole);
            }
        } else {
            this.kept.computeIfAbsent(place, key -> new ArrayList<>(1)).add(
                Map.entry(run.getValue(), run.getKey())
            );
        }
        return Map.entry(run.getKey(), done);
    }

    private Optional<Copy> held(
        final String place, final Map.Entry<String, String> hole,
        final Optional<Copy> guess, final int left, final List<Boolean> lacking
    ) {
        final Map.Entry<Optional<Copy>, Boolean> shaped = this.shape(hole);
        final Optional<Copy> next = shaped.getKey();
        final Optional<Copy> found;
        if (next.isEmpty() || next.equals(guess)) {
            found = next;
            lacking.add(shaped.getValue());
        } else if (left == 0) {
            found = Optional.empty();
        } else {
            this.guesses.guess(place, next);
            found = this.held(place, hole, next, left - 1, lacking);
        }
        return found;
    }

    private Map.Entry<Optional<Copy>, Boolean> shape(final Map.Entry<String, String> hole) {
        final List<Optional<Copy>> found = this.found(hole);
        final boolean ends = found.stream().anyMatch(
            each -> each.filter(Copy::stops).isPresent()
        );
        final List<Optional<Copy>> live = found.stream().filter(
            each -> each.map(copy -> !copy.stops()).orElse(true)
        ).collect(Collectors.toList());
        final boolean vacant = this.vacant(hole);
        final Optional<Copy> held;
        if (!ends && (vacant || live.isEmpty())) {
            held = Optional.empty();
        } else if (live.isEmpty()) {
            held = Optional.of(Copy.terminator());
        } else {
            held = this.known(live).map(copy -> copy.ending(copy.ends() || ends));
        }
        return Map.entry(
            held, vacant && !ends && (live.isEmpty() || this.known(live).isPresent())
        );
    }

    private boolean vacant(final Map.Entry<String, String> hole) {
        return this.bare.contains(hole.getKey())
            || this.copies.getOrDefault(hole.getKey(), List.of()).stream().anyMatch(
                call -> this.calls.leaves(call, hole.getKey(), hole.getValue())
            );
    }

    private List<Optional<Copy>> found(final Map.Entry<String, String> hole) {
        final Lookups seen = this.lookups();
        final List<Optional<Copy>> found = new ArrayList<>(0);
        for (final String call : this.copies.getOrDefault(hole.getKey(), List.of())) {
            if (!this.calls.leaves(call, hole.getKey(), hole.getValue())) {
                this.filler(call, hole, seen).ifPresent(found::add);
            }
        }
        for (final Annotation type : this.calls.made(hole.getKey())) {
            found.add(this.calls.member(type, hole.getValue()).map(Copy::new));
        }
        return found;
    }

    private Optional<Copy> known(final List<Optional<Copy>> live) {
        final Set<String> forms = live.stream().flatMap(Optional::stream)
            .flatMap(copy -> copy.candidates().stream())
            .map(Copy::formation).collect(Collectors.toCollection(TreeSet::new));
        final Optional<Copy> found;
        if (!forms.isEmpty()
            && live.stream().allMatch(Optional::isPresent)
            && forms.stream().allMatch(this.attributes::containsKey)) {
            found = Optional.of(
                new Copy(forms).ending(
                    live.stream().flatMap(Optional::stream).anyMatch(Copy::ends)
                )
            );
        } else {
            found = Optional.empty();
        }
        return found;
    }

    private Optional<Optional<Copy>> filler(
        final String call, final Map.Entry<String, String> hole,
        final Lookups seen
    ) {
        final Optional<String> arg = this.calls.argument(call, hole.getKey(), hole.getValue());
        final Map.Entry<Optional<Copy>, Set<String>> run = this.guesses.tracked(
            () -> arg.flatMap(place -> this.lookup.apply(place, seen))
        );
        final Optional<Optional<Copy>> found;
        if (run.getKey().filter(Copy::loops).isPresent()
            || run.getKey().isEmpty() && arg.isPresent() && this.guesses.blind(run.getValue())) {
            found = Optional.empty();
        } else {
            found = Optional.of(run.getKey());
        }
        return found;
    }
}
