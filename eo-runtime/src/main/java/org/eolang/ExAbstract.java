/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.Objects;

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
     *
     * @param cause Exception cause
     */
    public ExAbstract(final String cause) {
        this(cause, null);
    }

    /**
     * Ctor.
     *
     * @param root Root cause exception
     */
    public ExAbstract(final Throwable root) {
        this(Objects.toString(root, null), root);
    }

    /**
     * Ctor.
     *
     * @param cause Exception cause
     * @param root Root cause exception
     */
    public ExAbstract(final String cause, final Throwable root) {
        this(cause, root, true);
    }

    /**
     * Ctor.
     *
     * @param cause Exception cause
     * @param root Root cause exception
     * @param stack Whether the Java stack trace is worth recording, which it
     *  is not for an exception thrown as control flow
     */
    public ExAbstract(final String cause, final Throwable root, final boolean stack) {
        super(cause, root, true, stack);
    }
}
