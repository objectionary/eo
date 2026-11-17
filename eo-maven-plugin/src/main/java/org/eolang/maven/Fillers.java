/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.eolang.inference.Annotation;

/**
 * What the calls of the program and the atoms put into a void, when the
 * code alone says that.
 *
 * <p>Every call that copies the formation puts an argument into the void,
 * and every atom that returns a copy of it with brackets puts there the type
 * its brackets give. The void holds one of the formations these copy, when each of them
 * copies a formation the code fixes, all those formations have the same
 * voids in the same order, and no place copies the formation without a call.
 * With one formation the copy is sharp, and with several it is blurred.</p>
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
    private final BiFunction<String, Map<Map.Entry<?, ?>, Optional<Copy>>, Optional<Copy>> lookup;

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
        final BiFunction<String, Map<Map.Entry<?, ?>, Optional<Copy>>, Optional<Copy>> copy
    ) {
        this.attributes = places;
        this.calls = applications;
        this.copies = made;
        this.bare = empty;
        this.guesses = new Guesses();
        this.lookup = copy;
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
     * What a void holds, once what it is taken to hold stops changing.
     *
     * <p>It takes at most eight rounds, and after that the void is said to
     * hold nothing the code fixes.</p>
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
        this.guesses.guess(place, Optional.empty());
        final Map.Entry<Optional<Copy>, Set<String>> run = this.guesses.tracked(
            () -> this.held(place, hole, Optional.empty(), 8)
        );
        this.guesses.close(place);
        return Map.entry(run.getKey(), !this.guesses.leaning(run.getValue()));
    }

    private Optional<Copy> held(
        final String place, final Map.Entry<String, String> hole,
        final Optional<Copy> guess, final int left
    ) {
        final Optional<Copy> next = this.shape(hole);
        final Optional<Copy> found;
        if (next.isEmpty() || next.equals(guess)) {
            found = next;
        } else if (left == 0) {
            found = Optional.empty();
        } else {
            this.guesses.guess(place, next);
            found = this.held(place, hole, next, left - 1);
        }
        return found;
    }

    private Optional<Copy> shape(final Map.Entry<String, String> hole) {
        final Map<Map.Entry<?, ?>, Optional<Copy>> seen = new HashMap<>(0);
        final List<Optional<Copy>> found = this.copies.getOrDefault(
            hole.getKey(), List.of()
        ).stream().flatMap(call -> this.filler(call, hole, seen)).collect(Collectors.toList());
        for (final Annotation type : this.calls.made(hole.getKey())) {
            found.add(this.calls.member(type, hole.getValue()).map(Copy::new));
        }
        final Set<String> forms = found.stream().flatMap(Optional::stream)
            .map(Copy::formation).collect(Collectors.toCollection(TreeSet::new));
        final Optional<Copy> held;
        if (!this.bare.contains(hole.getKey()) && this.known(found, forms)) {
            held = Optional.of(
                new Copy(
                    new Annotation(forms.iterator().next()),
                    forms.size() == 1 && found.stream().allMatch(copy -> copy.get().sole())
                )
            );
        } else {
            held = Optional.empty();
        }
        return held;
    }

    private boolean known(final List<Optional<Copy>> found, final Set<String> forms) {
        return !forms.isEmpty()
            && found.stream().allMatch(Optional::isPresent)
            && forms.stream().allMatch(this.attributes::containsKey)
            && forms.stream().map(this.calls::voids).distinct().count() == 1L;
    }

    private Stream<Optional<Copy>> filler(
        final String call, final Map.Entry<String, String> hole,
        final Map<Map.Entry<?, ?>, Optional<Copy>> seen
    ) {
        final Optional<String> arg = this.calls.argument(call, hole.getKey(), hole.getValue());
        final Map.Entry<Optional<Copy>, Set<String>> run = this.guesses.tracked(
            () -> arg.flatMap(place -> this.lookup.apply(place, seen))
        );
        final Stream<Optional<Copy>> found;
        if (run.getKey().isEmpty() && arg.isPresent() && this.guesses.blind(run.getValue())) {
            found = Stream.empty();
        } else {
            found = Stream.of(run.getKey());
        }
        return found;
    }
}
