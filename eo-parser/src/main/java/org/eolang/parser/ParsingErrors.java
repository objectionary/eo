/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
import java.util.LinkedList;
import java.util.List;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.cactoos.Text;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Accumulates all parsing errors.
 * @since 0.30.0
 */
final class ParsingErrors extends BaseErrorListener
    implements ANTLRErrorListener, Iterable<Directive> {
    /**
     * Errors accumulated.
     */
    private final List<ParsingException> errors;

    /**
     * The source.
     */
    private final List<Text> lines;

    /**
     * Ctor.
     * @param lines The source in lines
     */
    ParsingErrors(final Text... lines) {
        this(new ListOf<>(lines));
    }

    /**
     * Ctor.
     * @param src The source in lines
     */
    ParsingErrors(final List<Text> src) {
        this.errors = new LinkedList<>();
        this.lines = src;
    }

    // @checkstyle ParameterNumberCheck (10 lines)
    @Override
    public void syntaxError(
        final Recognizer<?, ?> recognizer,
        final Object symbol,
        final int line,
        final int position,
        final String msg,
        final RecognitionException error
    ) {
        this.errors.add(
            new ParsingException(
                String.format(
                    "[%d:%d] %s: \"%s\"",
                    line, position, msg,
                    // @checkstyle AvoidInlineConditionalsCheck (1 line)
                    this.lines.size() < line ? "EOF" : this.lines.get(line - 1)
                ),
                error,
                line
            )
        );
    }

    @Override
    public Iterator<Directive> iterator() {
        return new org.cactoos.iterable.Joined<>(
            new Mapped<Iterable<Directive>>(
                error -> new Directives()
                    .xpath("/program/errors")
                    .add("error")
                    .attr("line", error.line())
                    .attr("severity", "critical")
                    .set(error.getMessage())
                    .up(),
                this.errors
            )
        ).iterator();
    }

    /**
     * How many errors?
     * @return Count of errors accumulated
     */
    public int size() {
        return this.errors.size();
    }
}
