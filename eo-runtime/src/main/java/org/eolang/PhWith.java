/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * A attr-putting object.
 *
 * @since 0.1
 */
public final class PhWith extends PhOnce {

    /**
     * Ctor.
     *
     * @param phi The object
     * @param name The name of attr
     * @param attr The value
     */
    public PhWith(final Phi phi, final String name, final Phi attr) {
        super(
            () -> {
                phi.put(name, attr);
                return phi;
            }
        );
    }

    /**
     * Ctor.
     *
     * @param phi The object
     * @param pos The position
     * @param attr The value
     */
    public PhWith(final Phi phi, final int pos, final Phi attr) {
        super(
            () -> {
                phi.put(pos, attr);
                return phi;
            }
        );
    }

}
