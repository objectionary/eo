/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The copy of an object that carries a \rho, kept together with the object
 * it was made from, so that the very same copy can be handed out again for
 * as long as the object being bound stays the one it was made from.
 * @since 0.73.4
 */
final class WithRho {

    /**
     * The object the copy was made from.
     */
    private final Phi origin;

    /**
     * The copy that carries the \rho.
     */
    private final Phi copy;

    /**
     * Ctor.
     * @param phi The object the copy was made from
     * @param bound The copy that carries the \rho
     */
    WithRho(final Phi phi, final Phi bound) {
        this.origin = phi;
        this.copy = bound;
    }

    /**
     * Was this copy made from the object given?
     * @param phi The object to compare with
     * @return TRUE if it was made from it
     */
    boolean made(final Phi phi) {
        return this.origin == phi;
    }

    /**
     * The copy that carries the \rho.
     * @return The copy
     */
    Phi phi() {
        return this.copy;
    }
}
