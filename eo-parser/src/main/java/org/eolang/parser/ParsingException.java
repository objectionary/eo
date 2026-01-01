/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * When parsing fails.
 *
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
     * @param line The place
     * @param msgs Messages
     */
    ParsingException(final int line, final String... msgs) {
        this(new IllegalStateException("Parsing error"), line, List.of(msgs));
    }

    /**
     * Ctor.
     * @param cause Cause of failure
     * @param line The place
     * @param msgs Messages
     */
    ParsingException(final Exception cause, final int line, final String... msgs) {
        this(cause, line, List.of(msgs));
    }

    /**
     * Ctor.
     *
     * @param cause The cause
     * @param line The place
     * @param msgs Messages
     * @since 0.1
     */
    ParsingException(final Exception cause, final int line, final List<String> msgs) {
        this(cause, line, String.join("\n", msgs));
    }

    /**
     * Ctor.
     *
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
