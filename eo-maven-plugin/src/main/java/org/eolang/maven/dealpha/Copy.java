/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven.dealpha;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;
import java.util.stream.Collectors;
import org.eolang.inference.Annotation;

/**
 * What a place copies: a formation, and where the objects in the voids of
 * the copy come from, when the code alone says that.
 *
 * <p>Java makes the copy an atom returns, and the type the atom says it
 * returns, such as {@code Φ.posix.status-return(Φ.number,Φ.string)}, gives
 * in brackets what is in each void. A call written at one place of the
 * program makes a copy whose voids hold the arguments written there, read in
 * the copy the call is written in. A copy of anything else has voids nobody
 * here can see into.</p>
 *
 * <p>A copy keeps only the type of the copy its call is written in. When that
 * one was made by a call too, its type is just its formation, so an argument
 * that reads one of its voids finds nothing. The program has finitely many
 * such types, so a place reads into finitely many copies and every read
 * ends.</p>
 *
 * <p>A copy may also be blurred: its formation stands for several formations,
 * such as the two the program puts into the {@code fork} of {@code bool}. It
 * keeps all those formations, its candidates, since each of them binds its
 * attributes in its own way: a name read off it is looked up in every
 * candidate, made by the same call, and copies a formation only when what
 * they give agrees (#9973). The candidates do not have to have the same
 * voids. When they do, the copy is uniform, and it tells where the arguments
 * of a call on it land. When they do not, as with {@code Φ.number} and
 * {@code Φ.number.plus} in a void filled by both, the copy may be read but
 * not called, since an argument at one position lands in different voids of
 * different candidates (#9978).</p>
 *
 * <p>A terminator makes a copy of no formation at all, as
 * {@link #terminator()} says. A name read off it, or a call on it, is a
 * terminator too, since the program never gets that far (#9982).</p>
 *
 * <p>A copy may also end: it is of its formations, or else it is the
 * terminator, as a void is that one call fills with {@code T} and another
 * with {@code [message]} (#9997). A name read off it is looked up in its
 * formations, since the terminator never gets that far. A call on it, though,
 * may land in the terminator, which takes its message by position only, so
 * the code does not fix where its arguments land, and the copy a call on it
 * makes ends too.</p>
 *
 * @since 0.76.0
 */
final class Copy {

    /**
     * The type: the formation, and the types an atom gives its voids in
     * brackets.
     */
    private final Annotation type;

    /**
     * The call that made the copy, when the code shows one: its place, and
     * the type of the copy it is written in.
     */
    private final Optional<Map.Entry<String, Annotation>> call;

    /**
     * The formations the copy may be of, in their natural order: just one
     * when the copy is sharp, and several when it is blurred.
     */
    private final Set<String> forms;

    /**
     * Whether it may be the terminator instead of any of its formations.
     */
    private final boolean end;

    /**
     * Ctor.
     *
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     */
    Copy(final Annotation kind) {
        this(kind, Optional.empty());
    }

    /**
     * Ctor.
     *
     * @param candidates The formations the copy may be of: one for a sharp
     *  copy, several for a blurred one, which may have different voids
     */
    Copy(final Collection<String> candidates) {
        this(
            new Annotation(new TreeSet<>(candidates).first()), Optional.empty(),
            new TreeSet<>(candidates), false
        );
    }

    /**
     * Ctor.
     *
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     * @param maker The call that made the copy, when the code shows one
     */
    Copy(final Annotation kind, final Optional<Map.Entry<String, Annotation>> maker) {
        this(kind, maker, new TreeSet<>(Set.of(kind.head())), false);
    }

    /**
     * Ctor.
     *
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     * @param maker The call that made the copy, when the code shows one
     * @param candidates The formations the copy may be of, which nobody
     *  changes afterwards
     * @param ends Whether it may be the terminator instead
     */
    private Copy(
        final Annotation kind, final Optional<Map.Entry<String, Annotation>> maker,
        final SortedSet<String> candidates, final boolean ends
    ) {
        this.type = kind;
        this.call = maker;
        this.forms = candidates;
        this.end = ends;
    }

    @Override
    public boolean equals(final Object other) {
        final boolean result;
        if (this == other) {
            result = true;
        } else if (other == null || this.getClass() != other.getClass()) {
            result = false;
        } else {
            result = this.type.equals(((Copy) other).type)
                && this.call.equals(((Copy) other).call)
                && this.forms.equals(((Copy) other).forms)
                && this.end == ((Copy) other).end;
        }
        return result;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.type, this.call, this.forms, this.end);
    }

    /**
     * The copy a terminator makes: it is of no formation at all, since the
     * program stops there and nothing is ever read off it (#9982).
     *
     * @return The copy
     */
    static Copy terminator() {
        return new Copy(new Annotation("⊥"), Optional.empty(), new TreeSet<>(), false);
    }

    /**
     * The copy a lookup gives when it is asked for again while it is still
     * running: it stands for whatever that lookup finds, so a side that is
     * this copy adds nothing to what the lookup finds (#9983).
     *
     * @return The copy
     */
    static Copy looped() {
        return new Copy(new Annotation("↺"), Optional.empty(), new TreeSet<>(), false);
    }

    /**
     * Whether it is what a terminator makes, so that the program never gets
     * past it.
     *
     * @return TRUE if it is
     */
    boolean stops() {
        return this.forms.isEmpty() && !this.loops();
    }

    /**
     * Whether it stands for a lookup that is still running, as
     * {@link #looped()} says.
     *
     * @return TRUE if it does
     */
    boolean loops() {
        return this.forms.isEmpty() && "↺".equals(this.type.head());
    }

    /**
     * Whether it may be the terminator instead of any of its formations, so
     * that a call on it fixes nothing about where its arguments land.
     *
     * @return TRUE if it may
     */
    boolean ends() {
        return this.end;
    }

    /**
     * The same copy, which may be the terminator instead, or not.
     *
     * @param ends Whether it may be the terminator instead
     * @return The copy
     */
    Copy ending(final boolean ends) {
        return new Copy(this.type, this.call, new TreeSet<>(this.forms), ends);
    }

    /**
     * Whether its formation is the only one it may be of, so that its
     * attributes may be read.
     *
     * @return TRUE if it is, FALSE if it is blurred
     */
    boolean sole() {
        return this.forms.size() == 1;
    }

    /**
     * Whether all the formations it may be of have the same voids in the
     * same order, so that the arguments of a call on it land in the same
     * voids whichever formation it is. A sharp copy always is.
     *
     * @param calls What the calls put into the voids
     * @return TRUE if they have
     */
    boolean uniform(final Calls calls) {
        return this.forms.stream().map(calls::voids).distinct().count() == 1L;
    }

    /**
     * The formation it copies.
     *
     * @return The locator of the formation
     */
    String formation() {
        return this.type.head();
    }

    /**
     * The sharp copies it may be, one for each formation it may be of, all
     * made by the call that made it.
     *
     * @return The copies, in the order of their formations
     */
    Collection<Copy> candidates() {
        return this.forms.stream().map(
            form -> new Copy(
                new Annotation(form), this.call, new TreeSet<>(Set.of(form)), false
            )
        ).collect(Collectors.toList());
    }

    /**
     * The copy that a call, written at a place in another copy, makes of
     * this one.
     *
     * @param place The place of the call
     * @param outer The copy the call is written in
     * @return The copy the call makes
     */
    Copy applied(final String place, final Copy outer) {
        return new Copy(
            new Annotation(this.formation()), Optional.of(Map.entry(place, outer.type)),
            new TreeSet<>(this.forms), this.end
        );
    }

    /**
     * The type that Java puts into a void, as the brackets of the type say.
     *
     * @param name The name of the void
     * @param calls What the calls put into the voids
     * @return The type, or nothing when the type has no brackets for the void
     */
    Optional<Annotation> member(final String name, final Calls calls) {
        return calls.member(this.type, name);
    }

    /**
     * The argument that the call which made the copy puts into a void: its
     * place, and the copy that place is read in.
     *
     * @param name The name of the void
     * @param calls What the calls put into the voids
     * @return The place and the copy, or nothing when no call in the program
     *  made the copy or the call does not show the argument
     */
    Optional<Map.Entry<String, Copy>> argument(final String name, final Calls calls) {
        return this.call.flatMap(
            made -> calls.argument(made.getKey(), this.formation(), name).map(
                arg -> Map.entry(arg, new Copy(made.getValue()))
            )
        );
    }

    /**
     * The copy that all of the given ones agree on.
     *
     * <p>A generic atom returns what one of several voids holds, and the code
     * does not say which. A name read off a blurred copy is what one of its
     * candidates gives, and the code does not say which either. So the
     * result may be of any formation that any of the given copies may be of.
     * When they are the same copy, it is that copy. When all those formations
     * have the same voids in the same order, it is a copy of them, sharp when
     * there is one and blurred when there are several, since the arguments of
     * a call on it land in the same voids whichever it is. It is made by the
     * call that made all the given copies, when there is one, and by no call
     * in particular otherwise. When one of them is unknown, or the formations
     * have different voids, nothing is said: {@code tuple.eq} and
     * {@code bytes.eq} name their voids differently, so the argument of a
     * call on one of the two lands nowhere in particular (#9973).</p>
     *
     * <p>A copy a terminator makes is left out, since the program stops
     * there and never reads anything off it: {@code x.if T 42} gives what
     * {@code 42} gives, and so does {@code recovered T 42}. When all of them
     * are terminators, so is the result (#9982).</p>
     *
     * <p>A copy that stands for a lookup still running, as {@link #looped()}
     * says, is left out too: it is the very lookup that asks, so it gives
     * only what that lookup finds, and the lookup has only to agree with the
     * others. When every one of them is such a copy, so is the result
     * (#9983).</p>
     *
     * @param held What each void holds
     * @param calls What the calls put into the voids
     * @return The copy they agree on, or nothing
     */
    static Optional<Copy> agreed(final Collection<Optional<Copy>> held, final Calls calls) {
        final List<Optional<Copy>> live = held.stream().filter(
            each -> each.map(copy -> !copy.stops() && !copy.loops()).orElse(true)
        ).collect(Collectors.toList());
        final Optional<Copy> found;
        if (!held.isEmpty() && live.isEmpty()
            && held.stream().flatMap(Optional::stream).allMatch(Copy::loops)) {
            found = Optional.of(Copy.looped());
        } else if (!held.isEmpty() && live.isEmpty()) {
            found = Optional.of(Copy.terminator());
        } else {
            found = Copy.joined(live, calls);
        }
        return found;
    }

    private static Optional<Copy> joined(
        final Collection<Optional<Copy>> held, final Calls calls
    ) {
        final Set<Copy> known = held.stream().flatMap(Optional::stream).collect(
            Collectors.toSet()
        );
        final SortedSet<String> forms = known.stream().flatMap(copy -> copy.forms.stream())
            .collect(Collectors.toCollection(TreeSet::new));
        final Set<Optional<Map.Entry<String, Annotation>>> makers = known.stream()
            .map(copy -> copy.call).collect(Collectors.toSet());
        final Optional<Copy> found;
        if (held.isEmpty() || held.contains(Optional.empty())
            || forms.stream().map(calls::voids).distinct().count() > 1L) {
            found = Optional.empty();
        } else if (known.size() == 1) {
            found = known.stream().findFirst().map(copy -> copy.ending(false));
        } else if (makers.size() == 1) {
            found = Optional.of(
                new Copy(new Annotation(forms.first()), makers.iterator().next(), forms, false)
            );
        } else {
            found = Optional.of(new Copy(forms));
        }
        return found;
    }
}
