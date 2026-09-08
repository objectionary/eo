/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * The marks a suffix leaves on the object it names.
 *
 * <p>A file-local handle (R-3.10.12) and a {@code !} const marker are written
 * the same way on every line shape that takes a suffix, and neither belongs to
 * the shape: the line decides what object is emitted, the suffix decides what
 * is written on it.</p>
 *
 * @since 0.74.0
 */
final class Marked {

    /**
     * The emitter.
     */
    private final Emit emit;

    /**
     * The suffix of the line.
     */
    private final Suffix suffix;

    /**
     * Ctor.
     *
     * @param target The emitter
     * @param tail The suffix of the line
     */
    Marked(final Emit target, final Suffix tail) {
        this.emit = target;
        this.suffix = tail;
    }

    /**
     * Write the marks onto the object most recently opened.
     */
    void apply() {
        if (!this.suffix.handle().isEmpty()) {
            this.emit.local(this.suffix.handle());
        }
        if (this.suffix.constant()) {
            this.emit.constant();
        }
    }
}
