/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Common exception.
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
     * @checkstyle ConstructorsCodeFreeCheck (5 lines)
     */
    public ExFailure(final String cause, final Object... args) {
        this(String.format(cause, args), (Throwable) null);
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
