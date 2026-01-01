/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Iterator;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
import org.apache.commons.text.StringEscapeUtils;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Source text of parser context.
 *
 * @since 0.34.0
 */
final class DrListing implements Iterable<Directive> {

    /**
     * Context.
     */
    private final ParserRuleContext context;

    /**
     * Ctor.
     * @param ctx Context
     */
    DrListing(final ParserRuleContext ctx) {
        this.context = ctx;
    }

    @Override
    public Iterator<Directive> iterator() {
        final String text = this.context.getStart().getInputStream().getText(
            new Interval(
                this.context.getStart().getStartIndex(),
                this.context.getStop().getStopIndex()
            )
        );
        return new Directives()
            .xpath("/object")
            .strict(1).add("listing")
            .set(StringEscapeUtils.escapeXml11(text))
            .iterator();
    }
}
