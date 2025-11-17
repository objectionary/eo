/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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

    /**
     * Ctor.
     *
     * @param phi The object
     * @param pos Position
     * @todo #4595:30min Remove unnecessary methods and constructors related to dispatch
     *  by index from eo-runtime. In previous PR we removed supporting dispatch by index from eo-parser.
     *  Now we can safely remove all the unnecessary logic from eo-runtime.
     */
    public PhMethod(final Phi phi, final int pos) {
        super(() -> phi.take(pos));
    }
}
