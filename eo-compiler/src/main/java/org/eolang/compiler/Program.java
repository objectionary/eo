/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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

import com.jcabi.log.Logger;
import com.jcabi.xml.XSLChain;
import com.jcabi.xml.XSLDocument;
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
import org.cactoos.list.ListOf;
import org.cactoos.list.Mapped;
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
     * @param file The file to write the XML to
     */
    public Program(final String nme, final Input ipt, final Path file) {
        this(nme, ipt, new OutputTo(file));
    }

    /**
     * Ctor.
     *
     * @param nme The name of it
     * @param ipt Input text
     * @param tgt Target
     */
    public Program(final String nme, final Input ipt, final Output tgt) {
        this.name = nme;
        this.input = ipt;
        this.target = tgt;
    }

    /**
     * Compile it to XML and save (with default set of XSLs).
     *
     * @throws IOException If fails
     */
    public void compile() throws IOException {
        this.compile(
            new ListOf<>(
                "errors/broken-aliases.xsl",
                "errors/duplicate-aliases.xsl",
                "errors/one-body.xsl",
                "errors/reserved-atoms.xsl",
                "errors/same-line-names.xsl",
                "errors/self-naming.xsl",
                "01-add-refs.xsl",
                "02-resolve-aliases.xsl",
                "errors/unknown-names.xsl",
                "03-abstracts-float-up.xsl",
                "04-rename-bases.xsl",
                "05-wrap-method-calls.xsl"
            )
        );
    }

    /**
     * Compile it to XML and save.
     *
     * @param xsls List of XSLs to apply
     * @throws IOException If fails
     */
    public void compile(final Iterable<String> xsls) throws IOException {
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
                        // @checkstyle AvoidInlineConditionalsCheck (1 line)
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
        final XeListener xel = new XeListener(this.name);
        new ParseTreeWalker().walk(xel, parser.program());
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(
                        new XSLChain(
                            new Mapped<>(
                                node -> new XSLDocument(
                                    Program.class.getResourceAsStream(node)
                                ),
                                xsls
                            )
                        ).transform(xel.xml()).toString()
                    ),
                    this.target
                )
            )
        ).value();
        Logger.debug(this, "Input of %d EO lines compiled", lines.length);
    }

}
