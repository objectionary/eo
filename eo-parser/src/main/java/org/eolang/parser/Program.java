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
package org.eolang.parser;

import com.jcabi.log.Logger;
import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.nio.file.Path;
import org.cactoos.Output;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;

/**
 * EO program.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
public final class Program {

    /**
     * XML to optimize.
     */
    private final XML source;

    /**
     * Target to save XMLs to.
     */
    private final Output target;

    /**
     * Ctor.
     *
     * @param dom XML to optimize
     * @param file The file to write the XML to
     */
    public Program(final XML dom, final Path file) {
        this(dom, new OutputTo(file));
    }

    /**
     * Ctor.
     *
     * @param dom XML to optimize
     * @param tgt Target
     */
    public Program(final XML dom, final Output tgt) {
        this.source = dom;
        this.target = tgt;
    }

    /**
     * Compile it to XMLs and save (with default set of XSLs).
     */
    public void compile() {
        this.compile(new Program.Spy.None());
    }

    /**
     * Compile it to XML and save (with default set of XSLs).
     *
     * @param spy The spy
     */
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public void compile(final Program.Spy spy) {
        this.compile(new Pack(), spy);
    }

    /**
     * Compile it to XML and save.
     *
     * @param xsls List of XSLs to apply
     */
    public void compile(final Iterable<XSL> xsls) {
        this.compile(xsls, new Program.Spy.None());
    }

    /**
     * Compile it to XML and save.
     *
     * @param xsls List of XSLs to apply
     * @param spy The spy
     */
    public void compile(final Iterable<XSL> xsls, final Program.Spy spy) {
        final XSL each = new XSLDocument(
            Program.class.getResourceAsStream("_each.xsl")
        ).with(new ClasspathSources(Program.class));
        int index = 0;
        XML before = this.source;
        for (final XSL xsl : xsls) {
            final XML after = each.with("step", index).transform(
                xsl.transform(before)
            );
            spy.push(index, xsl, after);
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
     * Spy.
     *
     * @since 0.1
     */
    public interface Spy {
        /**
         * New XSL produced.
         * @param index The index of the XSL
         * @param xsl The XSL used
         * @param xml The XML produced
         */
        void push(int index, XSL xsl, XML xml);

        /**
         * Empty spy.
         *
         * @since 0.1
         */
        final class None implements Program.Spy {
            @Override
            public void push(final int index, final XSL xsl, final XML xml) {
                // Nothing
            }
        }

        /**
         * Empty spy.
         *
         * @since 0.1
         */
        final class Verbose implements Program.Spy {
            @Override
            public void push(final int index, final XSL xsl, final XML xml) {
                Logger.debug(
                    this,
                    "Parsed #%d via %s\n%s",
                    index,
                    new XMLDocument(xsl.toString()).xpath("/*/@id").get(0),
                    xml
                );
            }
        }
    }

}
