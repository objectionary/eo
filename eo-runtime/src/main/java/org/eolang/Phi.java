/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * A simple object.
 *
 * <p>We call it Phi because of the name of the φ-calculus. Actually, a better
 * name would be "Object", but it's already occupied by Java. That's why
 * we call it Phi.</p>
 *
 * <p>Two Phi are equal when they are the same object. The hash code of a
 * Phi is its identity hash, which is not unique — two different objects
 * can share one — so equality is never decided from it.</p>
 *
 * @since 0.1
 */
public interface Phi extends Data, Term {

    /**
     * Lambda attribute.
     */
    String LAMBDA = "λ";

    /**
     * Phi attribute.
     */
    @SuppressWarnings("PMD.AvoidFieldNameMatchingTypeName")
    String PHI = "φ";

    /**
     * Rho attribute.
     */
    String RHO = "ρ";

    /**
     * The global scope object, which owns all other objects.
     * @checkstyle ConstantNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.FieldNamingConventions")
    Phi Φ = new PhPackage(PhPackage.GLOBAL);

    /**
     * Make a copy, leaving it at the same parent.
     * @return A copy
     */
    Phi copy();

    /**
     * Does this object still await a receiver?
     *
     * <p>True only when the object carries a rho attribute that nothing has
     * filled yet. An object that declares no rho wants no receiver, and one
     * whose rho is already bound must keep it, so both answer FALSE and a
     * dispatch leaves them alone.</p>
     *
     * @return TRUE if a rho attribute is present and empty
     */
    boolean needsRho();

    /**
     * Take object by name of the attribute.
     * @param name The name of the attribute
     * @return The object
     */
    Phi take(String name);

    /**
     * Put object by position of the attribute.
     * @param pos The position of the attribute
     * @param object The object to put
     */
    void put(int pos, Phi object);

    /**
     * Put object by name of the attribute.
     * @param name The name of the attribute
     * @param object The object to put
     */
    void put(String name, Phi object);

    /**
     * Get code locator of the phi.
     * @return String containing code locator
     */
    String locator();

    /**
     * Get forma of the phi.
     * @return Forma of it as {@link String}
     */
    String forma();

    /**
     * Resolve this object to its normal form, running dispatch, λ and φ but
     * without extracting its data.
     *
     * <p>The point is to reveal whether the object is a terminated
     * computation (a terminator) without forcing it: a terminator — whether
     * written as {@code T}, produced by an unset void, or returned by a
     * failing atom — surfaces here as a {@link PhTerminator} instance,
     * detectable by identity. A genuine, unrecoverable failure encountered
     * while resolving (a type violation, a missing Δ) propagates as an
     * {@link ExFailure}, exactly as it would during dataization.</p>
     *
     * @return The object in its normal form
     */
    Phi normalized();
}
