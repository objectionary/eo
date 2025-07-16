/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Abstract Phi object with attribute.
 *
 * @since 1.0
 */
abstract class AbstractPhWithAttr implements Phi {
    /**
     * Original phi object.
     */
    private final Phi original;

    /**
     * Ctor.
     * @param attr Original phi object
     */
    protected AbstractPhWithAttr(final Phi attr) {
        this.original = attr;
    }

    /**
     * Returns the original attribute.
     * @return The original attribute
     */
    Phi origin() {
        return this.original;
    }
}
