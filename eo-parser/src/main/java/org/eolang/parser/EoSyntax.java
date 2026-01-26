/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.util.List;
import java.util.function.Function;
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
 * Syntax parser that converts EO code to XMIR (XML-based Intermediate Representation) using ANTLR4.
 * EoSyntax parses EO source code, generates a structured XML representation, and applies a series
 * of transformations to produce canonical XMIR.
 *
 * <p>The parsing process includes lexical analysis, syntax analysis, and XML transformation:
 * 1. EO code is first processed by the EoIndentLexer
 * 2. Then parsed by ANTLR-generated parser
 * 3. Finally transformed into XMIR through a series of XSL transformations</p>
 *
 * <p>Usage examples:</p>
 *
 * <p>1. Parse EO code from a string:</p>
 * <pre>
 * XML xmir = new EoSyntax("[args] > app\n  42 > @").parsed();
 * </pre>
 *
 * <p>2. Parse EO code from a file:</p>
 * <pre>
 * XML xmir = new EoSyntax(
 *     new InputOf(new File("src/main/eo/fibonacci.eo"))
 * ).parsed();
 * </pre>
 *
 * <p>3. Parse with custom transformations:</p>
 * <pre>
 * XML xmir = new EoSyntax(
 *     "[x] > f\n  x > @",
 *     new TrClasspath<>(
 *         "/org/eolang/parser/pack/validation.xsl",
 *         "/org/eolang/parser/pack/transform.xsl"
 *     )
 * ).parsed();
 * </pre>
 *
 * <p>After parsing, errors can be found in the XML at the "/object/errors" XPath.
 * If no errors are present, the parsed program is valid EO code.</p>
 *
 * @since 0.1
 * @checkstyle ClassFanOutComplexityCheck (500 lines)
 */
public final class EoSyntax implements Syntax {
    /**
     * Set of optimizations that builds canonical XMIR from parsed EO.
     * @todo #3807:90min Refactor EoSyntax transformations to make them less coupled.
     *  Currently most of these transformations are strongly coupled.
     *  For example, `stars-to-tuples`, `StHex` and `explicit-data` are
     *  dependent on each other. Moreover, the order of transformations
     *  matters. We need to refactor these transformations to make them
     *  more independent and order-agnostic if possible.
     */
    private static final Function<XML, XML> CANONICAL = new Xsline(
        new TrFull(
            new TrJoined<>(
                new TrClasspath<>(
                    "/org/eolang/parser/parse/validate-before-stars.xsl",
                    "/org/eolang/parser/parse/resolve-before-stars.xsl",
                    "/org/eolang/parser/parse/wrap-method-calls.xsl",
                    "/org/eolang/parser/parse/const-to-dataized.xsl",
                    "/org/eolang/parser/parse/stars-to-tuples.xsl",
                    "/org/eolang/parser/parse/vars-float-up.xsl",
                    "/org/eolang/parser/parse/move-voids-up.xsl",
                    "/org/eolang/parser/parse/validate-objects-count.xsl",
                    "/org/eolang/parser/parse/build-fqns.xsl",
                    "/org/eolang/parser/parse/expand-qqs.xsl",
                    "/org/eolang/parser/parse/expand-aliases.xsl",
                    "/org/eolang/parser/parse/resolve-aliases.xsl",
                    "/org/eolang/parser/parse/add-default-package.xsl",
                    "/org/eolang/parser/parse/roll-bases.xsl",
                    "/org/eolang/parser/parse/cti-adds-errors.xsl",
                    "/org/eolang/parser/parse/decorate.xsl",
                    "/org/eolang/parser/parse/mandatory-as.xsl"
                ).back(),
                new TrDefault<>(new StHex())
            )
        )
    )::pass;

    /**
     * Text to parse.
     */
    private final Input input;

    /**
     * Transform XMIR after parsing.
     */
    private final Function<XML, XML> transform;

    /**
     * Ctor.
     * @param ipt The EO program to parse
     */
    public EoSyntax(final String ipt) {
        this(new InputOf(ipt));
    }

    /**
     * Ctor.
     * @param ipt The EO program to parse
     * @param transform Transform XMIR after parsing
     */
    public EoSyntax(final String ipt, final Train<Shift> transform) {
        this(new InputOf(ipt), transform);
    }

    /**
     * Ctor.
     * @param ipt The EO program to parse
     */
    public EoSyntax(final Input ipt) {
        this(ipt, EoSyntax.CANONICAL);
    }

    /**
     * Ctor for testing.
     * @param ipt The EO program to parse
     * @param transform Transform XMIR after parsing train
     */
    EoSyntax(final Input ipt, final Train<Shift> transform) {
        this(ipt, new Xsline(transform)::pass);
    }

    /**
     * Ctor.
     * @param ipt The EO program to parse
     * @param transform Transform XMIR after parsing function
     */
    EoSyntax(final Input ipt, final Function<XML, XML> transform) {
        this.input = ipt;
        this.transform = transform;
    }

    /**
     * Compile it to XML and save.
     *
     * <p>No exception will be thrown if the syntax is invalid. In any case, XMIR will
     * be generated and saved. Read it in order to find the errors,
     * at <tt>/object/errors</tt> XPath.</p>
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
        final XeEoListener xel = new XeEoListener();
        new ParseTreeWalker().walk(xel, parser.program());
        final XML dom = this.transform.apply(
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
                "The program of %d EO lines compiled, no errors",
                lines.size()
            );
        } else {
            Logger.debug(
                this, "The program of %d EO lines compiled with %d error(s)",
                lines.size(), errors
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
