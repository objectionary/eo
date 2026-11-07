/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Map;
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
    private final String type;

    /**
     * The place of the call that made the copy, or an empty string when the
     * code does not show one.
     */
    private final String call;

    /**
     * The type of the copy the call is written in.
     */
    private final String host;

    /**
     * Ctor.
     *
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     */
    Copy(final String kind) {
        this(kind, "", "Φ");
    }

    /**
     * Ctor.
     *
     * @param kind The type: the formation, and the types an atom gives its
     *  voids in brackets
     * @param place The place of the call that made the copy, or an empty
     *  string
     * @param outer The type of the copy the call is written in
     */
    Copy(final String kind, final String place, final String outer) {
        this.type = kind;
        this.call = place;
        this.host = outer;
    }

    @Override
    public String toString() {
        final String text;
        if (this.call.isEmpty()) {
            text = this.type;
        } else {
            text = String.join(" ", this.type, this.call, this.host);
        }
        return text;
    }

    /**
     * The formation it copies.
     *
     * @return The locator of the formation
     */
    String formation() {
        return new Annotation(this.type).head();
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
        return new Copy(this.formation(), place, outer.type);
    }

    /**
     * Where the object in a void comes from: its place, and the copy that
     * place is read in.
     *
     * @param name The name of the void
     * @param calls What the calls put into the voids
     * @return The place and the copy, or nothing when the code does not
     *  show them
     */
    Optional<Map.Entry<String, Copy>> filler(final String name, final Calls calls) {
        final Optional<Map.Entry<String, Copy>> found;
        if (this.call.isEmpty()) {
            found = calls.member(new Annotation(this.type), name).map(
                member -> Map.entry(member, this)
            );
        } else {
            found = calls.argument(this.call, this.formation(), name).map(
                arg -> Map.entry(arg, new Copy(this.host))
            );
        }
        return found;
    }
}
