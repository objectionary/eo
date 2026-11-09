/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
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
     * @param maker The call that made the copy, when the code shows one
     */
    Copy(final Annotation kind, final Optional<Map.Entry<String, Annotation>> maker) {
        this.type = kind;
        this.call = maker;
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
                && this.call.equals(((Copy) other).call);
        }
        return result;
    }

    @Override
    public int hashCode() {
        return Objects.hash(this.type, this.call);
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
            new Annotation(this.formation()), Optional.of(Map.entry(place, outer.type))
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
}
