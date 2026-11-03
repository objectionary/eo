/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * The name a void attribute is kept under.
 *
 * <p>The parser puts a mark in front of the name of every void (#9788).
 * Other objects can still fill the void, by position or by name, but they
 * cannot read it. This class is the only place in the runtime that knows
 * the mark. An atom declares its voids with
 * {@link Attr#Attr(String)} and reads them with
 * {@link PhDefault#free(String)}, both by the names its EO source gives
 * them.</p>
 *
 * @since 0.78.0
 */
final class Hidden {

    /**
     * The name of the void, as the EO source spells it.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param name The name of the void, as the EO source spells it
     */
    Hidden(final String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "v🌵".concat(this.name);
    }
}
