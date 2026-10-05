/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * This exception is used to enhance the error message
 * in the {@link Expect#it()} method.
 *
 * @since 0.51
 */
final class ExOtherwise extends RuntimeException {

    /**
     * Ctor.
     *
     * @param message Error message
     * @param root The exception that caused this one
     */
    ExOtherwise(final String message, final Throwable root) {
        super(message, root);
    }
}
