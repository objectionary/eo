/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Locale;
import org.cactoos.Text;

/**
 * Auto name for abstract object.
 *
 * @since 0.57.4
 */
final class AutoName implements Text {

    /**
     * The line number.
     */
    private final int line;

    /**
     * The indent in line.
     */
    private final int indent;

    /**
     * Ctor.
     *
     * @param lne Line number
     * @param ind Indent number
     */
    AutoName(final int lne, final int ind) {
        this.line = lne;
        this.indent = ind;
    }

    @Override
    public String asString() {
        return String.format(Locale.ROOT, "a🌵%d-%d", this.line, this.indent);
    }
}
