/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * A computation in its normal form, or the terminator it ended as.
 *
 * <p>A termination arrives in two shapes. It is a {@link PhTerminator} when
 * nothing forced it on the way, and it is an {@link ExFailure} when something
 * did: a {@code seq} step, a const, or anything else that dataizes while the
 * normal form is computed. Both are one termination, so both come out of here
 * as a terminator carrying the same reason.</p>
 *
 * @since 0.0.0
 */
final class Resolved {

    /**
     * The computation.
     */
    private final Phi origin;

    /**
     * Ctor.
     *
     * @param phi The computation to resolve
     */
    Resolved(final Phi phi) {
        this.origin = phi;
    }

    /**
     * The normal form, or the terminator the resolution ended as.
     *
     * @return The object
     */
    Phi it() {
        Phi picked;
        try {
            picked = this.origin.normalized();
        } catch (final ExFailure ex) {
            picked = new PhTerminator(new Data.ToPhi(ex.getMessage()));
        }
        return picked;
    }
}
