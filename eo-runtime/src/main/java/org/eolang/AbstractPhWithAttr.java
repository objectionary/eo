/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

abstract class AbstractPhWithAttr implements Phi {
    private final Phi origin;

    /**
     * Ctor.
     * @param attr Attribute
     */
    protected AbstractPhWithAttr(Phi attr) {
        this.origin = attr;
    }

    /**
     * Returns the original attribute.
     * @return The original attribute
     */
    public Phi origin() {
        return this.origin;
    }
}
