/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Common exception.
 *
 * @since 0.21
 */
public class ExFailure extends ExAbstract {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = 597748425437017615L;

    /**
     * Ctor.
     * @param cause Exception cause
     * @param args Arguments for {@link String#format(String, Object...)}
     */
    public ExFailure(final String cause, final Object... args) {
        super(String.format(cause, args));
    }

    /**
     * Ctor.
     * @param cause Exception cause
     * @param root Cause exception
     */
    public ExFailure(final String cause, final Throwable root) {
        super(cause, root);
    }
}
