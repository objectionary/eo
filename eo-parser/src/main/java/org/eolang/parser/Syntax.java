/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import org.cactoos.list.ListOf;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.FormattedText;
import org.cactoos.text.Joined;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.xembly.Xembler;

/**
 * Syntax parser, from EO to XMIR, using ANTLR4.
 *
 * @since 0.1
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
     * Checks redundant parentheses.
     */
    private final RedundantParentheses redundancy;

    /**
     * Ctor.
     *
     * @param nme The name of it
     * @param ipt Input text
     * @param tgt Target
     * @todo #485:90min The {@link org.eolang.parser.Syntax} by default just write warning to the
     *  log about redundant parentheses because some objects from objectioanry already contain
     *  redundancy parentheses in their source code that causes exceptions during parsing if
     *  {@link org.eolang.parser.Syntax#redundancy} is true. By that reason it's important to
     *  fix all redundancy parentheses in the objectionary. Then we will be able to remove
     *  'redundancy' flag and check all eo programs for redundant parentheses by default.
     */
    public Syntax(final String nme, final Input ipt, final Output tgt) {
        this(nme, ipt, tgt, new RedundantParentheses());
    }

    /**
     * Ctor.
     *
     * @param nme The name of it
     * @param ipt Input text
     * @param tgt Target
     * @param redundancy Check for redundant parentheses
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    public Syntax(
        final String nme,
        final Input ipt,
        final Output tgt,
        final RedundantParentheses redundancy
    ) {
        this.name = nme;
        this.input = ipt;
        this.target = tgt;
        this.redundancy = redundancy;
    }

    /**
     * Compile it to XML and save.
     *
     * @throws IOException If fails
     */
    public void parse() throws IOException {
        final List<Text> lines = this.lines();
        final ANTLRErrorListener errors = new BaseErrorListener() {
            // @checkstyle ParameterNumberCheck (10 lines)
            @Override
            public void syntaxError(final Recognizer<?, ?> recognizer,
                final Object symbol, final int line,
                final int position, final String msg,
                final RecognitionException error
            ) {
                throw new ParsingException(
                    String.format(
                        "[%d:%d] %s: \"%s\"",
                        line, position, msg,
                        // @checkstyle AvoidInlineConditionalsCheck (1 line)
                        lines.size() < line ? "EOF" : lines.get(line - 1)
                    ),
                    error,
                    line
                );
            }
        };
        final ProgramLexer lexer = new EoLexer(this.normalize());
        lexer.removeErrorListeners();
        lexer.addErrorListener(errors);
        final ProgramParser parser = new ProgramParser(
            new CommonTokenStream(lexer)
        );
        parser.removeErrorListeners();
        parser.addErrorListener(errors);
        final XeListener xel = new XeListener(this.name, this.redundancy);
        new ParseTreeWalker().walk(xel, parser.program());
        final XML dom = new XMLDocument(new Xembler(xel).domQuietly());
        new Schema(dom).check();
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(dom.toString()),
                    this.target
                )
            )
        ).value();
        Logger.debug(this, "Input of %d EO lines compiled", lines.size());
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

}
