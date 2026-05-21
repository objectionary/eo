/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * Thrown by a {@link Line} when a per-line or cross-line rule is
 * violated.
 *
 * <p>Carries the {@code line} and {@code pos} (R-9.1.1 / R-9.1.2) of the
 * failure plus the canonical message text from §9.9. The walker catches
 * this, calls {@link Emit#rollback(int)} with the line's savepoint
 * (R-7.2), and emits the error via {@link Emit#error(int, int, String)}.
 * Subsequent lines continue against the post-recovery state.</p>
 *
 * <p>Unchecked by design — every rule violation surfaces this way and
 * the walker is the single catch site. Forcing a checked declaration on
 * every {@code Line.into} would clutter every implementation for no
 * caller benefit. *
 *
 * @since 0.1
 */
final class ParseError extends RuntimeException {

    /**
     * Serialisation version.
     */
    private static final long serialVersionUID = 1L;

    /**
     * Source line (1-indexed).
     */
    private final int line;

    /**
     * Column position (0-indexed).
     */
    private final int pos;

    /**
     * Ctor.
     * @param row Source line
     * @param col Column position
     * @param message Canonical message text from §9.9
     */
    ParseError(final int row, final int col, final String message) {
        super(message);
        this.line = row;
        this.pos = col;
    }

    /**
     * Source line of the failure.
     * @return Line
     */
    int line() {
        return this.line;
    }

    /**
     * Column position of the failure.
     * @return Position
     */
    int pos() {
        return this.pos;
    }
}
