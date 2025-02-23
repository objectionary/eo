/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * A copy-making object.
 *
 * @since 0.1
 */
public final class PhCopy extends PhOnce {

    /**
     * Ctor.
     *
     * @param phi The object
     */
    public PhCopy(final Phi phi) {
        super(phi::copy);
    }
}
