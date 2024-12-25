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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.InputMismatchException;
import org.antlr.v4.runtime.NoViableAltException;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.Token;
import org.cactoos.Text;

/**
 * Accumulates all parsing errors related to EO parser.
 * @since 0.50
 */
final class EoParserErrors extends BaseErrorListener implements Iterable<ParsingException> {

    /**
     * Errors accumulated.
     */
    private final List<ParsingException> errors;

    /**
     * The source.
     */
    private final Lines lines;

    /**
     * Ctor.
     * @param src The source in lines
     */
    EoParserErrors(final List<Text> src) {
        this(new ArrayList<>(0), new Lines(src));
    }

    /**
     * Ctor.
     * @param errors Errors accumulated
     * @param lines The source in lines
     */
    private EoParserErrors(final List<ParsingException> errors, final Lines lines) {
        this.errors = errors;
        this.lines = lines;
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
        if (!Parser.class.isInstance(recognizer)) {
            throw new IllegalArgumentException(
                String.format(
                    "Illegal usage of %s, please use it to recognize only EO parser errors", this
                )
            );
        }
        if (error instanceof NoViableAltException || error instanceof InputMismatchException) {
            final Token token = (Token) symbol;
            final Parser parser = (Parser) recognizer;
            final String rule = parser.getRuleInvocationStack().get(0);
            final String[] names = parser.getRuleNames();
            final String detailed;
            if (names[EoParser.RULE_objects].equals(rule)) {
                detailed = "Invalid object declaration";
            } else if (names[EoParser.RULE_metas].equals(rule)) {
                detailed = "Invalid meta declaration";
            } else if (names[EoParser.RULE_program].equals(rule)) {
                detailed = "Invalid program declaration";
            } else {
                detailed = "no viable alternative at input";
            }
            this.errors.add(
                new ParsingException(
                    String.format(
                        "[%d:%d] %s: %s:%n%s",
                        line, position,
                        "error",
                        detailed,
                        new UnderlinedMessage(
                            this.lines.line(line),
                            position,
                            Math.max(token.getStopIndex() - token.getStartIndex(), 1)
                        ).formatted()
                    ),
                    error,
                    line
                )
            );
        } else if (Objects.isNull(error)) {
            this.errors.add(
                new ParsingException(
                    String.format(
                        "[%d:%d] %s: %s", line, position, "error", msg
                    ),
                    line
                )
            );
        } else {
            this.errors.add(
                new ParsingException(
                    String.format(
                        "[%d:%d] %s: \"%s\"",
                        line, position, msg, this.lines.line(line)
                    ),
                    error,
                    line
                )
            );
        }
    }

    @Override
    public Iterator<ParsingException> iterator() {
        return this.errors.iterator();
    }
}
