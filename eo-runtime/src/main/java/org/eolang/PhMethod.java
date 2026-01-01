/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * A method-calling object.
 *
 * @since 0.1
 */
public final class PhMethod extends PhOnce {

    /**
     * Ctor.
     *
     * @param phi The object
     * @param mtd The name of method
     */
    public PhMethod(final Phi phi, final String mtd) {
        super(() -> phi.take(mtd));
    }
}
