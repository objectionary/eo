/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Exception to control threads interruptions.
 *
 * @since 0.28.3
 */
public class ExInterrupted extends ExAbstract {
    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = 2377580403532765676L;

    /**
     * Ctor.
     * @param root Root cause exception
     */
    public ExInterrupted(final InterruptedException root) {
        super(root);
    }
}
