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
 * <p>It is guaranteed that the hash codes of different Phi are different,
 * and equal to the vertex.</p>
 *
 * @since 0.1
 */
public interface Phi extends Data {

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
     *
     * @checkstyle ConstantNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.FieldNamingConventions")
    Phi Φ = new PhPackage(PhPackage.GLOBAL);

    /**
     * Make a copy, leaving it at the same parent.
     *
     * @return A copy
     */
    Phi copy();

    /**
     * Returns true if object has bound rho attribute.
     * @return True if object has rho bound attribute
     */
    boolean hasRho();

    /**
     * Take object by name of the attribute.
     * @param name The name of the attribute
     * @return The object
     */
    Phi take(String name);

    /**
     * Put object by position of the attribute.
     * @param pos The position of the attribute.
     * @param object The object to put
     */
    void put(int pos, Phi object);

    /**
     * Put object by name of the attribute.
     * @param name The name of the attribute.
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
     * @return Forma of it as {@link String}.
     */
    String forma();

    /**
     * Copy self object.
     * @param self Self
     * @return Copy of self
     * @todo #3480:45min Remove Phi.copy(Phi) method from the interface.
     *  Currently, we use this method in {@link PhDefault#copy()} in order to copy the attributes
     *  correctly. However, not all Phi implementations need this type of copying. It should be
     *  possible to remove this method, and use only {@link Phi#copy()}.
     */
    Phi copy(Phi self);
}
