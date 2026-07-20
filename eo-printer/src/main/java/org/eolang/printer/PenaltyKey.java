/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

/**
 * A tunable weight of the pretty-printer's {@link Penalty} function.
 *
 * <p>Each key names one knob of the penalty-based layout algorithm and
 * carries the default that {@link Penalty} (and {@link Pretty}) falls back
 * to when a weight is not supplied. A caller may override any subset of
 * these by passing a {@code Map<PenaltyKey, Integer>} to {@link Pretty}
 * or {@link Xmir}; absent keys keep their {@link #fallback()} value, so
 * one aesthetic is no longer baked into the tool.</p>
 *
 * @since 0.57.0
 * @checkstyle MagicNumberCheck (46 lines)
 */
public enum PenaltyKey {

    /**
     * Points charged for each level of indentation on a line.
     */
    INDENT(2),

    /**
     * Points charged per opening parenthesis, progressively: the k-th
     * parenthesis on a line costs k times this weight.
     */
    BRACKET(19),

    /**
     * Points charged for each character past {@link #WIDTH}.
     */
    EXCESS(3),

    /**
     * Points charged for every symbol in the block.
     */
    SYMBOL(1),

    /**
     * Points charged for each space that applies an argument — every
     * space on a line beyond its leading indentation.
     */
    APPLICATION(7),

    /**
     * The column after which characters start being charged.
     */
    WIDTH(80),

    /**
     * The width of a single indentation level, in spaces.
     */
    STEP(2);

    /**
     * The value used when this key is not overridden.
     */
    private final int def;

    /**
     * Ctor.
     * @param fallback The default value of this key
     */
    PenaltyKey(final int fallback) {
        this.def = fallback;
    }

    /**
     * The default value of this key.
     * @return The value to use when the key is absent from the weights map
     */
    public int fallback() {
        return this.def;
    }
}
