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
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.cactoos.Text;
import org.cactoos.io.InputStreamOf;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Syntax parser, from Phi-calculus to XMIR, using ANTLR4.
 * @since 0.34.0
 */
public final class PhiSyntax implements Syntax {
    /**
     * Name of the program.
     */
    private final String name;

    /**
     * Input.
     */
    private final Text input;

    /**
     * Ctor for the tests.
     * @param input Input
     */
    PhiSyntax(final String input) {
        this("test", () -> input);
    }

    /**
     * Ctor.
     * @param nme Name of the program
     * @param inpt Input
     */
    public PhiSyntax(final String nme, final Text inpt) {
        this.name = nme;
        this.input = inpt;
    }

    @Override
    public XML parsed() throws IOException {
        final XePhiListener xel = new XePhiListener(this.name);
        final ParsingErrors spy = new ParsingErrors(this.input);
        final PhiLexer lexer = new PhiLexer(
            CharStreams.fromStream(
                new InputStreamOf(this.input)
            )
        );
        lexer.addErrorListener(spy);
        final PhiParser parser = new PhiParser(
            new CommonTokenStream(lexer)
        );
        parser.removeErrorListeners();
        parser.addErrorListener(spy);
        new ParseTreeWalker().walk(xel, parser.program());
        final XML dom = new Xsline(new StHash()).pass(
            new XMLDocument(
                new Xembler(
                    new Directives(xel).append(spy)
                ).domQuietly()
            )
        );
        new Schema(dom).check();
        if (spy.size() == 0) {
            Logger.debug(this, "Input of PHI calculus compiled, no errors");
        } else {
            Logger.debug(
                this, "Input of PHI calculus failed to compile (%d errors)",
                spy.size()
            );
        }
        return dom;
    }
}
