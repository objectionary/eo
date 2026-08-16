/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

/**
 * A line's naming suffix, as {@link Transition#apply} needs it: the
 * label to attach to the pushed-or-replaced level, and whether the line
 * shape is allowed to sit under an atom parent (R-3.10.13).
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
     * Ctor.
     * @param label The suffix's source name, or {@code null}
     * @param permitted Whether this line shape may sit under an atom parent
     */
    Admission(final String label, final boolean permitted) {
        this.label = label;
        this.permitted = permitted;
    }

    /**
     * The suffix's source name.
     * @return The label, or {@code null}
     */
    String label() {
        return this.label;
    }

    /**
     * Whether this line shape may sit under an atom parent.
     * @return Permitted flag
     */
    boolean permitted() {
        return this.permitted;
    }
}
