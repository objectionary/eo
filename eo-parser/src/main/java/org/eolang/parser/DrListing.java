/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import java.util.Iterator;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Interval;
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
            .xpath("/program")
            .strict(1).add("listing")
            .set(text)
            .iterator();
    }
}
