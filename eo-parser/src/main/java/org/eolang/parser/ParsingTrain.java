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

import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.StAfter;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.TrBulk;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.TrLogged;
import org.cactoos.iterable.Mapped;

/**
 * Train of XSL shifts.
 *
 * @since 0.1
 */
public final class ParsingTrain extends TrEnvelope {

    /**
     * Apply changes to each XML after processing.
     */
    private static final XSL EACH = new XSLDocument(
        ParsingTrain.class.getResourceAsStream("_each.xsl")
    ).with(new ClasspathSources(ParsingTrain.class));

    /**
     * Sheets in the right order.
     */
    private static final String[] SHEETS = {
        "/org/eolang/parser/errors/not-empty-atoms.xsl",
        "/org/eolang/parser/errors/middle-varargs.xsl",
        "/org/eolang/parser/errors/duplicate-names.xsl",
        "/org/eolang/parser/errors/many-free-attributes.xsl",
        "/org/eolang/parser/errors/broken-aliases.xsl",
        "/org/eolang/parser/errors/duplicate-aliases.xsl",
        "/org/eolang/parser/errors/global-nonames.xsl",
        "/org/eolang/parser/errors/same-line-names.xsl",
        "/org/eolang/parser/errors/self-naming.xsl",
        "/org/eolang/parser/add-refs.xsl",
        "/org/eolang/parser/wrap-method-calls.xsl",
        "/org/eolang/parser/vars-float-up.xsl",
        "/org/eolang/parser/add-refs.xsl",
        "/org/eolang/parser/warnings/unsorted-metas.xsl",
        "/org/eolang/parser/expand-aliases.xsl",
        "/org/eolang/parser/resolve-aliases.xsl",
        "/org/eolang/parser/add-default-package.xsl",
        "/org/eolang/parser/errors/broken-refs.xsl",
        "/org/eolang/parser/errors/unknown-names.xsl",
        "/org/eolang/parser/errors/noname-attributes.xsl",
        "/org/eolang/parser/errors/duplicate-names.xsl",
        "/org/eolang/parser/warnings/duplicate-metas.xsl",
        "/org/eolang/parser/errors/unused-aliases.xsl",
        "/org/eolang/parser/errors/data-objects.xsl",
        "/org/eolang/parser/warnings/unit-test-without-phi.xsl",
    };

    /**
     * Ctor.
     */
    public ParsingTrain() {
        super(
            new TrBulk<>(
                new TrLambda(
                    new TrLogged(new TrDefault<>()),
                    shift -> new StAfter(
                        shift,
                        new StLambda(
                            shift::uid,
                            (pos, xml) -> ParsingTrain.EACH.with("step", pos)
                                .with("sheet", shift.uid())
                                .transform(xml)
                        )
                    )
                )
            ).with(new Mapped<>(StClasspath::new, ParsingTrain.SHEETS)).back()
        );
    }

}
