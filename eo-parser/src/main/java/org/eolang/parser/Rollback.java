/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;

/**
 * A joint savepoint over the {@link Emit} sink and the {@link Stack}
 * levels, taken before a recoverable parse and rolled back together
 * on a {@link ParseError} (§7).
 *
 * @since 0.1
 */
final class Rollback {

    /** The stack to restore on {@link #apply()}. */
    private final Stack stack;

    /** The sink to roll back on {@link #apply()}. */
    private final Emit emit;

    /** The sink savepoint taken at construction. */
    private final Savepoint token;

    /** The stack levels snapshot taken at construction. */
    private final List<Level> frame;

    /**
     * Ctor.
     *
     * @param stack The stack to snapshot and later restore
     * @param emit The sink to mark and later roll back
     * @param token The sink savepoint already taken
     * @param frame The stack levels snapshot already taken
     */
    Rollback(
        final Stack stack, final Emit emit, final Savepoint token, final List<Level> frame
    ) {
        this.stack = stack;
        this.emit = emit;
        this.token = token;
        this.frame = frame;
    }

    /** Roll the sink and the stack back to this savepoint. */
    void apply() {
        this.emit.rollback(this.token);
        this.stack.restore(this.frame);
    }
}
