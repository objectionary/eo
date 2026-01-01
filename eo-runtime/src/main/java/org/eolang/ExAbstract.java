/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Abstract exception.
 *
 * <p>The exception raised when something is not right inside
 * attributes.</p>
 *
 * @since 0.21
 */
public abstract class ExAbstract extends RuntimeException {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = 597749420437007615L;

    /**
     * Ctor.
     * @param cause Exception cause
     */
    public ExAbstract(final String cause) {
        super(cause);
    }

    /**
     * Ctor.
     * @param cause Exception cause
     * @param root Root cause exception
     */
    public ExAbstract(final String cause, final Throwable root) {
        super(cause, root);
    }

    /**
     * Ctor.
     * @param root Root cause exception
     */
    public ExAbstract(final Throwable root) {
        super(root);
    }
}
