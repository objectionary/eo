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

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.util.List;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.cactoos.Input;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.list.ListOf;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.FormattedText;
import org.cactoos.text.Joined;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Syntax parser, from EO to XMIR, using ANTLR4.
 *
 * @since 0.1
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 */
public final class EoSyntax implements Syntax {
    /**
     * The name of the EO program being parsed, usually the name of
     * <tt>.eo</tt> file itself. This name will be present in the
     * generated XML. This is done for the sake of traceability: it will
     * always be possible to tell which XMIR file was generated from
     * which EO program.
     */
    private final String name;

    /**
     * Text to parse.
     */
    private final Input input;

    /**
     * Ctor.
     *
     * @param ipt The EO program to parse
     */
    public EoSyntax(final Input ipt) {
        this("unknown", ipt);
    }

    /**
     * Ctor.
     * @param ipt The EO program to parse
     */
    public EoSyntax(final String ipt) {
        this("unknown", ipt);
    }

    /**
     * Ctor.
     *
     * @param nme The name of the EO program being parsed
     * @param ipt The EO program to parse
     */
    public EoSyntax(final String nme, final String ipt) {
        this(nme, new InputOf(ipt));
    }

    /**
     * Ctor.
     *
     * @param nme The name of the EO program being parsed
     * @param ipt The EO program to parse
     */
    public EoSyntax(final String nme, final Input ipt) {
        this.name = nme;
        this.input = ipt;
    }

    /**
     * Compile it to XML and save.
     *
     * <p>No exception will be thrown if the syntax is invalid. In any case, XMIR will
     * be generated and saved. Read it in order to find the errors,
     * at <tt>/program/errors</tt> XPath.</p>
     *
     * @return Parsed XML
     * @throws IOException If fails
     */
    public XML parsed() throws IOException {
        final List<Text> lines = this.lines();
        final GeneralErrors spy = new GeneralErrors(lines);
        final EoLexer lexer = new EoIndentLexer(this.normalize());
        lexer.removeErrorListeners();
        lexer.addErrorListener(spy);
        final EoParser parser = new EoParser(
            new CommonTokenStream(lexer)
        );
        parser.removeErrorListeners();
        final EoParserErrors eospy = new EoParserErrors(lines);
        parser.addErrorListener(eospy);
        final XeEoListener xel = new XeEoListener(this.name);
        new ParseTreeWalker().walk(xel, parser.program());
        final XML dom = Syntax.CANONICAL.pass(
            new XMLDocument(
                new Xembler(
                    new Directives(xel).append(new DrErrors(spy)).append(new DrErrors(eospy))
                ).domQuietly()
            )
        );
        final long errors = new Unchecked<>(new LengthOf(spy)).value()
            + new Unchecked<>(new LengthOf(eospy)).value();
        if (errors == 0) {
            Logger.debug(
                this,
                "The %s program of %d EO lines compiled, no errors",
                this.name, lines.size()
            );
        } else {
            Logger.debug(
                this, "The %s program of %d EO lines compiled with %d error(s)",
                this.name, lines.size(), errors
            );
        }
        return dom;
    }

    /**
     * Normalize input to UNIX format to ensure that EOL exists at the
     * end of the text.
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
