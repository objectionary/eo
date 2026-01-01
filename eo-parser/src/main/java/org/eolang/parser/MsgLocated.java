/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Error message that includes the location of the error.
 * @since 0.50
 */
final class MsgLocated {

    /**
     * The line where the error occurred.
     */
    private final int line;

    /**
     * The position in the line where the error occurred.
     */
    private final int position;

    /**
     * The error message.
     */
    private final String message;

    /**
     * Ctor.
     * @param line The line where the error occurred.
     * @param position The position in the line where the error occurred.
     * @param message The error message.
     */
    MsgLocated(final int line, final int position, final String message) {
        this.line = line;
        this.position = position;
        this.message = message;
    }

    /**
     * Formats the error message.
     * @return The formatted error message.
     */
    String formatted() {
        return String.format("[%d:%d] error: '%s'", this.line, this.position, this.message);
    }
}
