/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XML;
import java.util.Collection;
import java.util.HashSet;

/**
 * Which form of type the links table gives an object.
 *
 * <p>A row of that table holds one element and the element says what kind of
 * answer it is: a datum, a termination, a variable, or an object this one is a
 * copy of. Asked about an object, this gives back the same form, so that a
 * type worked out over there can be written down again over here.</p>
 *
 * <p>It also gives the name that form goes by, and the two are not the same
 * question. Every literal of a program has a locator of its own and all of
 * them are one type, so a datum is kept by a name no locator can have —
 * locators begin at the root of the program — and two fillings that arrive at
 * a datum are one filling.</p>
 *
 * @since 0.69.0
 */
final class Forms {

    /**
     * The objects that are data.
     */
    private final Collection<String> ground;

    /**
     * The objects that terminate.
     */
    private final Collection<String> dead;

    /**
     * The objects that are voids.
     */
    private final Collection<String> free;

    /**
     * Ctor.
     * @param links The links table, as {@link Resolved} left it
     */
    Forms(final XML links) {
        this(
            new HashSet<>(links.xpath("/links/type[data]/@id")),
            new HashSet<>(links.xpath("/links/type[terminator]/@id")),
            new HashSet<>(links.xpath("/links/type[var]/@id"))
        );
    }

    /**
     * Ctor.
     * @param data The objects that are data
     * @param terminators The objects that terminate
     * @param voids The objects that are voids
     */
    Forms(
        final Collection<String> data,
        final Collection<String> terminators,
        final Collection<String> voids
    ) {
        this.ground = data;
        this.dead = terminators;
        this.free = voids;
    }

    /**
     * The name the form of this object goes by.
     * @param object The locator of the object
     * @return The name, which is the locator itself unless every object of
     *  that form is one type
     */
    String name(final String object) {
        final String found;
        if (this.ground.contains(object)) {
            found = "data";
        } else if (this.dead.contains(object)) {
            found = "terminator";
        } else {
            found = object;
        }
        return found;
    }

    /**
     * The form of this object, to be written somewhere else.
     * @param object The locator of the object
     * @return The type
     */
    Type type(final String object) {
        final Type found;
        if (this.ground.contains(object)) {
            found = new Data();
        } else if (this.dead.contains(object)) {
            found = new Terminator();
        } else if (this.free.contains(object)) {
            found = new Var(object);
        } else {
            found = new Ref(object);
        }
        return found;
    }
}
