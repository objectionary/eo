/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * What an object of a program turns out to be.
 *
 * <p>There are several kinds of answer and there will be more: an object is
 * a copy of another one, or a datum, or a termination, or a choice between
 * several objects. They have nothing in common except where they are written
 * — inside the row of the object they are about — so that is all this says,
 * and a new kind is a new class rather than another column of a table nobody
 * can keep straight.</p>
 *
 * <p>What comes back leaves the cursor where it found it, since a row holds
 * one type and the row that follows starts beside it.</p>
 *
 * <p>A type is also asked which object of the program it names, and most of
 * them name none: a datum names the ground every literal stands on, a
 * variable names a void nobody has looked into, a choice names several at
 * once. Only a copy has a locator a reader can go and look at, so only a copy
 * gives one back, and asking is how a reader learns whether there is anything
 * to go and look at without having to know which kind of type it is holding.</p>
 *
 * @since 0.69.0
 */
@FunctionalInterface
interface Type {

    /**
     * This type as the contents of a row.
     * @return The directives
     */
    Directives directives();

    /**
     * The object of the program this type names.
     * @return The locator, empty where the type names no one object
     */
    default String names() {
        return "";
    }
}
