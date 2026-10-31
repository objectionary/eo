/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * The exception for case when hash not found.
 *
 * @since 0.28.11
 */
final class HashNotFoundException extends RuntimeException {

    /**
     * The main constructor.
     *
     * @param cause The cause of it
     */
    HashNotFoundException(final String cause) {
        super(cause);
    }
}
