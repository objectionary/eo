/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * One argument of one call, against the object the call was made on.
 *
 * <p>A call made on a void says something no name taken off that void can
 * say: that whatever fills it is applied at all, at which place, and with
 * what. {@code while} writes {@code ^.body index} and {@code malloc} writes
 * {@code ^.scope m}, and the arity of those calls and what they pass used to
 * leave no row anywhere (#8158).</p>
 *
 * <p>What was passed is kept as a type rather than as a locator, because a
 * locator is a place in one program and a type is something a reader can go
 * and look at. An argument is written afresh at every call site, so the same
 * object passed at eleven of them is eleven locators and one type, and
 * counting the locators apart would make one call made eleven times look like
 * eleven calls.</p>
 *
 * @since 0.72.0
 */
final class Call {

    /**
     * The object the call was made on.
     */
    private final String applied;

    /**
     * The place the argument fills.
     */
    private final int slot;

    /**
     * What was passed there.
     */
    private final Type carried;

    /**
     * Ctor.
     * @param object The locator of the object the call was made on
     * @param place The place the argument fills, counted from zero
     * @param passed What was passed there
     */
    Call(final String object, final int place, final Type passed) {
        this.applied = object;
        this.slot = place;
        this.carried = passed;
    }

    /**
     * The object the call was made on.
     * @return The locator
     */
    String of() {
        return this.applied;
    }

    /**
     * This call, to be put inside the row of a void.
     * @return The directives
     */
    Directives directives() {
        return new Directives()
            .add("apply")
            .attr("of", this.applied)
            .attr("place", this.slot)
            .append(this.carried.directives())
            .up();
    }
}
