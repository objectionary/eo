/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A line's naming suffix, as {@link Transition#apply} needs it: names
 * the pushed-or-replaced level when the line carries a name suffix, and
 * says whether the line shape is allowed to sit under an atom parent
 * (R-3.10.13).
 * @since 0.1
 */
final class Admission {

    /**
     * The suffix's source name, or {@code null} when the line carries
     * no name suffix.
     */
    private final String label;

    /**
     * Whether this line shape may sit under an atom parent.
     */
    private final boolean permitted;

    /**
     * Whether the line declares an atom of its own.
     */
    private final boolean own;

    /**
     * Ctor.
     * @param label The suffix's source name, or {@code null}
     * @param permitted Whether this line shape may sit under an atom parent
     */
    Admission(final String label, final boolean permitted) {
        this(label, permitted, false);
    }

    /**
     * Ctor.
     * @param label The suffix's source name, or {@code null}
     * @param permitted Whether this line shape may sit under an atom parent
     * @param own Whether the line declares an atom of its own
     */
    Admission(final String label, final boolean permitted, final boolean own) {
        this.label = label;
        this.permitted = permitted;
        this.own = own;
    }

    /**
     * Name the level with this suffix's label, when there is one.
     * @param level The level to name
     */
    void name(final Level level) {
        if (this.label != null) {
            level.name(this.label);
        }
    }

    /**
     * Whether this line shape may sit under an atom parent.
     * @return Permitted flag
     */
    boolean permitted() {
        return this.permitted;
    }

    /**
     * The message for a line this atom body cannot hold — R-6.3.4 (b)
     * gives a nested atom its own text, since telling the author that
     * the child is not a test says nothing about the nesting.
     * @return Canonical message
     */
    String violation() {
        final String message;
        if (this.own) {
            message = "atom may not contain a nested atom";
        } else {
            message = "atom may contain only test attributes";
        }
        return message;
    }
}
