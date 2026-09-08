/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.cactoos.Text;

/**
 * The XMIR name a void parameter is emitted under.
 *
 * <p>A parameter written as {@code @} declares the formation's decoratee
 * and is emitted as {@code φ}; one written as {@code ^} declares its
 * receiver and is emitted as {@code ρ} (R-3.4.2 / R-3.4.11 / R-9.3).
 * Every other token names itself. The §9.3 table is the single source of
 * truth for these promotions, so every parameter loop that emits a void
 * asks this object rather than deciding for itself.</p>
 *
 * @since 0.74.0
 */
final class VoidName implements Text {

    /**
     * The parameter, as the source wrote it.
     */
    private final String raw;

    /**
     * Ctor.
     * @param token The parameter, as the source wrote it
     */
    VoidName(final String token) {
        this.raw = token;
    }

    @Override
    public String asString() {
        final String mapped;
        if ("@".equals(this.raw)) {
            mapped = "φ";
        } else if ("^".equals(this.raw)) {
            mapped = "ρ";
        } else {
            mapped = this.raw;
        }
        return mapped;
    }
}
