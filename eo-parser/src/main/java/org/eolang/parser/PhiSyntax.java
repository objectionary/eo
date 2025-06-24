/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
 * Syntax parser that converts Phi-calculus notation to XMIR (XML-based Intermediate Representation)
 * using ANTLR4. PhiSyntax parses Phi-calculus code and transforms it into structured XML
 * that represents the program in a normalized form.
 *
 * <p>The Phi-calculus is a formal model of computation that serves as the theoretical foundation
 * for the EO language. This parser converts Phi-calculus notation with its specialized
 * symbols (like ⟦, ⟧, ↦, ⤍, etc.) into XMIR that can be further processed.</p>
 *
 * <p>The parsing process includes lexical analysis, syntax analysis, and XML transformations:
 * 1. Phi code is tokenized by the PhiLexer
 * 2. Then parsed by the ANTLR-generated PhiParser
 * 3. Finally transformed into canonical XMIR through a series of XSL transformations</p>
 *
 * <p>Usage examples:</p>
 *
 * <p>1. Parse basic Phi-calculus notation:</p>
 * <pre>
 * XML xmir = new PhiSyntax("{⟦obj ↦ ⟦x ↦ Φ.org.eolang.int⟧⟧}").parsed();
 * </pre>
 *
 * <p>2. Parse with a program name and add extra directives:</p>
 * <pre>
 * XML xmir = new PhiSyntax(
 *     "factorial",
 *     () -> "{⟦fact ↦ ⟦n ↦ Φ.org.eolang.int⟧⟧}",
 *     new Directives().xpath("/object").attr("version", "1.0")
 * ).parsed();
 * </pre>
 *
 * <p>3. Parse with custom transformations:</p>
 * <pre>
 * XML xmir = new PhiSyntax(
 *     "{⟦x ↦ ⟦y ↦ Φ.org.eolang.float⟧⟧}",
 *     new TrClasspath<>(
 *         "/org/eolang/parser/unphi/specialized-transform.xsl"
 *     )
 * ).parsed();
 * </pre>
 *
 * <p>After parsing, any syntax errors can be found in the XML at the "/object/errors" XPath.
 * If no errors are present, the parsed program is valid Phi-calculus notation.</p>
 *
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
                "/org/eolang/parser/parse/validate-objects-count.xsl",
                "/org/eolang/parser/unphi/wrap-bytes.xsl",
                "/org/eolang/parser/parse/roll-bases.xsl"
            ).back()
        )
    )::pass;

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
        this(() -> input, new Directives(), new Xsline(train)::pass);
    }

    /**
     * Ctor for the tests.
     * @param input Input
     */
    public PhiSyntax(final Text input) {
        this(input, new Directives());
    }

    /**
     * Ctor.
     * @param inpt Input
     * @param extra Extra directives to append
     */
    public PhiSyntax(final Text inpt, final Iterable<Directive> extra) {
        this(inpt, extra, CANONICAL);
    }

    /**
     * Base ctor.
     * @param inpt Input
     * @param extra Extra directives to append
     * @param transform Functions that transforms XMIR after parsing
     */
    private PhiSyntax(
        final Text inpt,
        final Iterable<Directive> extra,
        final Function<XML, XML> transform
    ) {
        this.input = inpt;
        this.extra = extra;
        this.transform = transform;
    }

    @Override
    public XML parsed() throws IOException {
        final XePhiListener xel = new XePhiListener();
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
