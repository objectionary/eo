/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.Map;

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
     * The form every object was given, from {@link Pairs}.
     */
    private final Map<String, String> given;

    /**
     * Ctor.
     * @param forms The form every object was given by the links table, by the
     *  locator of the object
     */
    Forms(final Map<String, String> forms) {
        this.given = forms;
    }

    /**
     * The name the form of this object goes by.
     * @param object The locator of the object
     * @return The name, which is the locator itself unless every object of
     *  that form is one type
     */
    String name(final String object) {
        final String form = this.given.getOrDefault(object, "");
        final String found;
        if ("data".equals(form) || "terminator".equals(form)) {
            found = form;
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
        final String form = this.given.getOrDefault(object, "");
        final Type found;
        if ("data".equals(form)) {
            found = new Data();
        } else if ("terminator".equals(form)) {
            found = new Terminator();
        } else if ("var".equals(form)) {
            found = new Var(object);
        } else {
            found = new Ref(object);
        }
        return found;
    }
}
