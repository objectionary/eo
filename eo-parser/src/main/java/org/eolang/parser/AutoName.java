/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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
     * Extra symbol.
     */
    private final String extra;

    AutoName(final int lne, final int pos) {
        this(lne, pos, "");
    }

    AutoName(final ParserRuleContext context) {
        this(context, "");
    }

    AutoName(final ParserRuleContext context, final String extra) {
        this(context.getStart().getLine(), context.getStart().getCharPositionInLine(), extra);
    }

    AutoName(final int lne, final int pos, final String extr) {
        this.line = lne;
        this.position = pos;
        this.extra = extr;
    }

    @Override
    public String asString() {
        return String.format("a%s\uD83C\uDF35%d%d", this.extra, this.line, this.position);
    }
}
