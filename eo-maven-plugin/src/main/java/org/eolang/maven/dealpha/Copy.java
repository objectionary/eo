/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven.dealpha;

import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
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
 * <p>A copy may also be blurred: its formation stands for several formations
 * that have the same voids in the same order, such as the two the program
 * puts into the {@code fork} of {@code bool}. Such a copy tells where the
 * arguments of a call on it land, but nothing may be read off it, since each
 * of those formations binds its attributes in its own way.</p>
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
     * Whether the formation is the only one the copy may be of.
     */
    private final boolean exact;

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
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     * @param sole Whether the formation is the only one the copy may be of,
     *  and not one of several with the same voids
     */
    Copy(final Annotation kind, final boolean sole) {
        this(kind, Optional.empty(), sole);
    }

    /**
     * Ctor.
     *
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     * @param maker The call that made the copy, when the code shows one
     */
    Copy(final Annotation kind, final Optional<Map.Entry<String, Annotation>> maker) {
        this(kind, maker, true);
    }

    /**
     * Ctor.
     *
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     * @param maker The call that made the copy, when the code shows one
     * @param sole Whether the formation is the only one the copy may be of
     */
    Copy(
        final Annotation kind, final Optional<Map.Entry<String, Annotation>> maker,
        final boolean sole
    ) {
        this.type = kind;
        this.call = maker;
        this.exact = sole;
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
                && this.exact == ((Copy) other).exact;
        }
        return result;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.type, this.call, this.exact);
    }

    /**
     * Whether its formation is the only one it may be of, so that its
     * attributes may be read.
     *
     * @return TRUE if it is, FALSE if it is blurred
     */
    boolean sole() {
        return this.exact;
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
            this.exact
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
     * does not say which. The result is known only when every void holds a
     * copy of the same formation. When they are the same copy, it is that
     * copy. When they are copies of the same formation made by different
     * calls, it is a copy of the formation that no call in particular made.
     * When one of them is unknown or blurred, or two of them copy different
     * formations, nothing is said.</p>
     *
     * @param held What each void holds
     * @return The copy they agree on, or nothing
     */
    static Optional<Copy> agreed(final Collection<Optional<Copy>> held) {
        final Set<Copy> known = held.stream().flatMap(Optional::stream).collect(
            Collectors.toSet()
        );
        final Optional<Copy> found;
        if (held.isEmpty() || held.contains(Optional.empty())
            || known.stream().map(Copy::formation).distinct().count() > 1L) {
            found = Optional.empty();
        } else if (known.size() == 1) {
            found = known.stream().findFirst();
        } else if (known.stream().allMatch(Copy::sole)) {
            found = Optional.of(new Copy(new Annotation(known.iterator().next().formation())));
        } else {
            found = Optional.empty();
        }
        return found;
    }
}
