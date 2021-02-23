/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import org.cactoos.Output;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.io.TeeInput;
import org.cactoos.iterable.Joined;
import org.cactoos.list.ListOf;
import org.cactoos.list.Mapped;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.TextOf;

/**
 * Pipeline of XSL transformations.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
public final class Xsline {

    /**
     * XML to optimize.
     */
    private final XML input;

    /**
     * Where to save XML.
     */
    private final Output target;

    /**
     * XSLs to use.
     */
    private final Iterable<XSL> xsls;

    /**
     * The spy to use.
     */
    private final Spy spy;

    /**
     * Ctor.
     *
     * @param dom XML to optimize
     * @param tgt Target
     * @param aspy The spy
     */
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public Xsline(final XML dom, final Output tgt, final Spy aspy) {
        this(
            dom, tgt, aspy,
            new ListOf<>(
                "org/eolang/parser/errors/not-empty-atoms.xsl",
                "org/eolang/parser/errors/middle-varargs.xsl",
                "org/eolang/parser/errors/duplicate-names.xsl",
                "org/eolang/parser/errors/many-free-attributes.xsl",
                "org/eolang/parser/errors/broken-aliases.xsl",
                "org/eolang/parser/errors/duplicate-aliases.xsl",
                "org/eolang/parser/errors/global-nonames.xsl",
                "org/eolang/parser/errors/same-line-names.xsl",
                "org/eolang/parser/errors/self-naming.xsl",
                "org/eolang/parser/add-refs.xsl",
                "org/eolang/parser/wrap-method-calls.xsl",
                "org/eolang/parser/vars-float-up.xsl",
                "org/eolang/parser/add-refs.xsl",
                "org/eolang/parser/expand-aliases.xsl",
                "org/eolang/parser/resolve-aliases.xsl",
                "org/eolang/parser/add-default-package.xsl",
                "org/eolang/parser/errors/broken-refs.xsl",
                "org/eolang/parser/errors/unknown-names.xsl",
                "org/eolang/parser/errors/noname-attributes.xsl",
                "org/eolang/parser/errors/data-objects.xsl"
            )
        );
    }

    /**
     * Ctor.
     *
     * @param dom XML to optimize
     * @param tgt Target
     * @param aspy The spy
     * @param sheets List of XSL sheet names
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public Xsline(final XML dom, final Output tgt,
        final Spy aspy, final Iterable<String> sheets) {
        this(dom, tgt, Xsline.mapped(sheets), aspy);
    }

    /**
     * Ctor.
     *
     * @param dom XML to optimize
     * @param tgt Target
     * @param sheets List of XSL sheet names
     * @param aspy The spy
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private Xsline(final XML dom, final Output tgt,
        final Iterable<XSL> sheets, final Spy aspy) {
        this.input = dom;
        this.target = tgt;
        this.xsls = sheets;
        this.spy = aspy;
    }

    /**
     * Add these sheets to the list.
     * @param sheets Names of them
     * @return New object
     */
    @SuppressWarnings("unchecked")
    public Xsline with(final Iterable<String> sheets) {
        return new Xsline(
            this.input, this.target,
            new Joined<>(this.xsls, Xsline.mapped(sheets)),
            this.spy
        );
    }

    /**
     * Compile it to XML and save to output.
     *
     * @throws IOException If fails
     */
    public void pass() throws IOException {
        final XSL each = new XSLDocument(
            Xsline.class.getResourceAsStream("_each.xsl")
        ).with(new ClasspathSources(Xsline.class));
        int index = 0;
        XML before = this.input;
        for (final XSL xsl : this.xsls) {
            final XML dom = new XMLDocument(xsl.toString());
            final XML after = each.with("step", index)
                .with("sheet", dom.xpath("/*/@id").get(0))
                .transform(xsl.transform(before));
            this.spy.push(index, xsl, after);
            ++index;
            before = after;
        }
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(before.toString()),
                    this.target
                )
            )
        ).value();
    }

    /**
     * Map strings to XSL objects.
     * @param sheets Names
     * @return Objects
     */
    private static Iterable<XSL> mapped(final Iterable<String> sheets) {
        return new Mapped<>(
            doc -> new XSLDocument(
                new TextOf(
                    new ResourceOf(doc)
                ).asString()
            ).with(new ClasspathSources()),
            sheets
        );
    }

}
