/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The exception raised when trying to get() an attribute,
 * which is still abstract.
 *
 * @since 0.21
 */
public final class ExUnset extends ExFailure {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = 597748420437017615L;

    /**
     * Ctor.
     * @param cause Cause description
     */
    public ExUnset(final String cause) {
        super(cause);
    }

    /**
     * Ctor.
     * @param cause Cause description
     * @param root Caused exception
     */
    public ExUnset(final String cause, final Throwable root) {
        super(cause, root);
    }
}
