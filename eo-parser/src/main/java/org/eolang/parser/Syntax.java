/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.cactoos.Input;
import org.cactoos.Output;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.io.TeeInput;
import org.cactoos.iterable.Mapped;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.FormattedText;
import org.cactoos.text.Joined;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.xembly.Directive;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Syntax parser, from EO to XMIR, using ANTLR4.
 *
 * @since 0.1
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 */
public final class Syntax {

    /**
     * The name of it.
     */
    private final String name;

    /**
     * Text to parse.
     */
    private final Input input;

    /**
     * Target to save XML to.
     */
    private final Output target;

    /**
     * Ctor.
     *
     * @param nme The name of it
     * @param ipt Input text
     * @param tgt Target
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    public Syntax(
        final String nme,
        final Input ipt,
        final Output tgt
    ) {
        this.name = nme;
        this.input = ipt;
        this.target = tgt;
    }

    /**
     * Compile it to XML and save.
     *
     * <p>No exception will be thrown if the syntax is invalid. In any case, XMIR will
     * be generated and saved. Read it in order to find the errors,
     * at <tt>/program/errors</tt> XPath.</p>
     *
     * @throws IOException If fails
     */
    public void parse() throws IOException {
        final List<Text> lines = this.lines();
        final ParsingErrors spy = new ParsingErrors(lines);
        final ProgramLexer lexer = new EoLexer(this.normalize());
        lexer.removeErrorListeners();
        lexer.addErrorListener(spy);
        final ProgramParser parser = new ProgramParser(
            new CommonTokenStream(lexer)
        );
        parser.removeErrorListeners();
        parser.addErrorListener(spy);
        final XeListener xel = new XeListener(this.name);
        new ParseTreeWalker().walk(xel, parser.program());
        final XML dom = new XMLDocument(
            new Xembler(
                new Directives(xel).append(spy)
            ).domQuietly()
        );
        new Schema(dom).check();
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(dom.toString()),
                    this.target
                )
            )
        ).value();
        if (spy.size() == 0) {
            Logger.debug(this, "Input of %d EO lines compiled, no errors", lines.size());
        } else {
            Logger.debug(
                this, "Input of %d EO lines failed to compile (%d errors)",
                lines.size(), spy.size()
            );
        }
    }

    /**
     * Normalize input to UNIX format.
     * Ensure EOL at EOF.
     *
     * @return UNIX formatted text.
     */
    private Text normalize() {
        return new FormattedText(
            "%s\n",
            new Joined(new TextOf("\n"), this.lines())
        );
    }

    /**
     * Split input into lines.
     * @return Lines without line breaks.
     */
    private List<Text> lines() {
        return new ListOf<>(new Split(new TextOf(this.input), "\r?\n"));
    }

    /**
     * Accumulates all parsing errors.
     *
     * @since 0.30.0
     */
    private static final class ParsingErrors extends BaseErrorListener
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
         * @param src The source in lines
         */
        private ParsingErrors(final List<Text> src) {
            this.errors = new LinkedList<>();
            this.lines = src;
        }

        // @checkstyle ParameterNumberCheck (10 lines)
        @Override
        public void syntaxError(final Recognizer<?, ?> recognizer,
            final Object symbol, final int line,
            final int position, final String msg,
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

}
