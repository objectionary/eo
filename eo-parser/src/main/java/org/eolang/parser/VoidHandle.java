/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.cactoos.Text;

/**
 * The file-local handle a void parameter carries, if any.
 *
 * <p>The receiver of a formation is a void named {@code ρ}, written as
 * {@code ^} in the bracket head (R-3.4.11). Written as {@code ^name}
 * instead, it also gets a readable name (R-3.4.13): {@code name} becomes a
 * file-local handle (R-3.10.12) that the body may use wherever it would
 * otherwise write {@code ^}. This object is that handle, and it is empty
 * for every other parameter, which carries none.</p>
 *
 * @since 0.64.0
 */
final class VoidHandle implements Text {

    /**
     * The parameter, as the source wrote it.
     */
    private final String raw;

    /**
     * Ctor.
     *
     * @param token The parameter, as the source wrote it
     */
    VoidHandle(final String token) {
        this.raw = token;
    }

    @Override
    public String asString() {
        final String handle;
        if (this.raw.startsWith("^")) {
            handle = this.raw.substring(1);
        } else {
            handle = "";
        }
        return handle;
    }
}
