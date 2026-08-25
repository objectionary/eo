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
 * <p>The hash code of a Phi is its identity hash, which is not unique:
 * two different objects can share one (see #7304).</p>
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
     * Is this object still free of a receiver?
     *
     * <p>True while nothing has been bound yet, whether the object carries an
     * empty rho attribute or declares none at all. Both are templates that a
     * dispatch must instantiate, so it hands out a fresh copy and offers it a
     * receiver, which an object declaring no rho declines. An object whose rho
     * is already bound is no template: it answers FALSE and a dispatch passes
     * it along untouched.</p>
     *
     * @return TRUE if no rho has been bound
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
