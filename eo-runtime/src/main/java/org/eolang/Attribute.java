/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Attribute.
 * @since 0.1
 */
public interface Attribute extends Term {

    /**
     * Make a copy of it.
     * @param self The object that this attribute will belong to
     * @return A copy
     */
    Attribute copy(Phi self);

    /**
     * Take the object out.
     *
     * <p>If attribute is not set - throws {@link ExUnset}.</p>
     *
     * @return The object
     */
    Phi get();

    /**
     * Put a new object in.
     * @param phi The object to put
     */
    void put(Phi phi);

    /**
     * Is this a void attribute that hasn't been set yet?
     *
     * <p>A vacant attribute is an empty slot ready to receive a value through
     * positional application. Attributes that already hold a value, and
     * attributes that are not void at all, are not vacant.</p>
     *
     * @return TRUE if it's an unset void attribute
     */
    boolean vacant();
}
