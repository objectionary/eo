/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.antlr.v4.runtime.ParserRuleContext;
import org.cactoos.Text;

/**
 * Auto name for abstract object.
 * @since 0.57.4
 */
final class AutoName implements Text {

    /**
     * The line number.
     */
    private final int line;

    /**
     * The Position in line.
     */
    private final int position;

    /**
     * Ctor.
     * @param context Context
     */
    AutoName(final ParserRuleContext context) {
        this(context.getStart().getLine(), context.getStart().getCharPositionInLine());
    }

    /**
     * Ctor.
     * @param lne Line number
     * @param pos Position number
     */
    AutoName(final int lne, final int pos) {
        this.line = lne;
        this.position = pos;
    }

    @Override
    public String asString() {
        return String.format("a\uD83C\uDF35%d%d", this.line, this.position);
    }
}
