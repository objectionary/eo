/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * When parsing fails.
 * @since 0.1
 */
final class ParsingException extends RuntimeException {

    /**
     * Serialization marker.
     */
    private static final long serialVersionUID = -3043426132301042201L;

    /**
     * The place.
     */
    private final int place;

    /**
     * Ctor.
     * @param cause Cause of failure
     * @param line The place
     * @param msg Message
     */
    ParsingException(final Exception cause, final int line, final String msg) {
        super(msg, cause);
        this.place = line;
    }

    /**
     * Get the place.
     * @return Line
     */
    int line() {
        return this.place;
    }
}
