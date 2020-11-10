/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package org.eolang.compiler;

import java.io.IOException;
import java.nio.file.Path;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.cactoos.Input;
import org.cactoos.Output;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.io.UncheckedInput;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.TextOf;

/**
 * Program.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
public final class Program {

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
     * @param ipt Input text
     * @param file The file to write the XML to
     */
    public Program(final Input ipt, final Path file) {
        this(ipt, new OutputTo(file));
    }

    /**
     * Ctor.
     *
     * @param ipt Input text
     * @param tgt Target
     */
    public Program(final Input ipt, final Output tgt) {
        this.input = ipt;
        this.target = tgt;
    }

    /**
     * Compile it to XML and save.
     *
     * @throws IOException If fails
     */
    public void compile() throws IOException {
        final String[] lines = new TextOf(this.input).asString().split("\n");
        final ANTLRErrorListener errors = new BaseErrorListener() {
            // @checkstyle ParameterNumberCheck (10 lines)
            @Override
            public void syntaxError(final Recognizer<?, ?> recognizer,
                final Object symbol, final int line,
                final int position, final String msg,
                final RecognitionException error) {
                throw new CompileException(
                    String.format(
                        "[%d:%d] %s: \"%s\"",
                        line, position, msg,
                        lines.length < line ? "EOF" : lines[line - 1]
                    ),
                    error
                );
            }
        };
        final ProgramLexer lexer =
            new ProgramLexer(
                CharStreams.fromStream(
                    new UncheckedInput(this.input).stream()
                )
            );
        lexer.removeErrorListeners();
        lexer.addErrorListener(errors);
        final ProgramParser parser =
            new ProgramParser(
                new CommonTokenStream(lexer)
            );
        parser.removeErrorListeners();
        parser.addErrorListener(errors);
        final XeListener xel = new XeListener();
        new ParseTreeWalker().walk(xel, parser.program());
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(xel.xml()),
                    this.target
                )
            )
        ).value();
//        Logger.info(this, "EO program parsed:\n%s", xel.xml());
    }

}
