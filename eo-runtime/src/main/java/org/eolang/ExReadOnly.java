/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * The exception raised when trying to put() an attribute,
 * which is read-only.
 *
 * @since 0.21
 */
public final class ExReadOnly extends ExAbstract {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = 697748420447017615L;

    /**
     * Ctor.
     * @param cause Cause description
     */
    public ExReadOnly(final String cause) {
        super(cause);
    }

    /**
     * Ctor.
     * @param cause Cause description
     * @param root Caused exception
     */
    public ExReadOnly(final String cause, final Throwable root) {
        super(cause, root);
    }

}
