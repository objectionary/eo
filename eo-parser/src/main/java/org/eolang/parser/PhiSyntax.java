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
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.util.function.Function;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.cactoos.Text;
import org.cactoos.io.InputStreamOf;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.xembly.Directive;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Syntax parser, from Phi-calculus to XMIR, using ANTLR4.
 * @since 0.34.0
 */
public final class PhiSyntax implements Syntax {
    /**
     * Set of optimizations that builds canonical XMIR from parsed PHI.
     */
    private static final Function<XML, XML> CANONICAL = new Xsline(
        new TrFull(
            new TrClasspath<>(
                "/org/eolang/parser/parse/move-voids-up.xsl",
                "/org/eolang/parser/parse/wrap-method-calls.xsl",
                "/org/eolang/parser/unphi/wrap-bytes.xsl",
                "/org/eolang/parser/unphi/normalize-bytes.xsl",
                "/org/eolang/parser/unphi/atoms-with-bound-attrs.xsl",
                "/org/eolang/parser/parse/roll-bases.xsl"
            ).back()
        )
    )::pass;

    /**
     * Name of the program.
     */
    private final String name;

    /**
     * Input.
     */
    private final Text input;

    /**
     * Extra directives to append to parsed phi.
     */
    private final Iterable<Directive> extra;

    /**
     * Transform XMIR after parsing.
     */
    private final Function<XML, XML> transform;

    /**
     * Ctor for the tests.
     * @param input Input
     */
    public PhiSyntax(final String input) {
        this(() -> input);
    }

    /**
     * Ctor for testing.
     * @param input Input
     * @param train Train of transformations to apply
     */
    PhiSyntax(final String input, final Train<Shift> train) {
        this("test", () -> input, new Directives(), new Xsline(train)::pass);
    }

    /**
     * Ctor for the tests.
     * @param input Input
     */
    public PhiSyntax(final Text input) {
        this("test", input, new Directives());
    }

    /**
     * Ctor.
     * @param nme Name of the program
     * @param inpt Input
     * @param extra Extra directives to append
     */
    public PhiSyntax(final String nme, final Text inpt, final Iterable<Directive> extra) {
        this(nme, inpt, extra, PhiSyntax.CANONICAL);
    }

    /**
     * Base ctor.
     * @param nme Name of the program
     * @param inpt Input
     * @param extra Extra directives to append
     * @param transform Functions that transforms XMIR after parsing
     */
    private PhiSyntax(
        final String nme,
        final Text inpt,
        final Iterable<Directive> extra,
        final Function<XML, XML> transform
    ) {
        this.name = nme;
        this.input = inpt;
        this.extra = extra;
        this.transform = transform;
    }

    @Override
    public XML parsed() throws IOException {
        final XePhiListener xel = new XePhiListener(this.name);
        final GeneralErrors spy = new GeneralErrors(this.input);
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
        final XML dom = this.transform.apply(
            new XMLDocument(
                new Xembler(
                    new Directives(xel).append(new DrErrors(spy)).append(this.extra)
                ).domQuietly()
            )
        );
        final long errors = new Unchecked<>(new LengthOf(spy)).value();
        if (errors == 0) {
            Logger.debug(this, "Input of PHI calculus compiled, no errors");
        } else {
            Logger.debug(
                this, "Input of PHI calculus failed to compile (%d errors)",
                errors
            );
        }
        return dom;
    }
}
