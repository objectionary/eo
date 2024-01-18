/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
import com.yegor256.xsline.StEndless;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.StSequence;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrFast;
import com.yegor256.xsline.TrJoined;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.TrLogged;
import java.util.logging.Level;

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
        ParsingTrain.class.getResourceAsStream("_each.xsl"),
        "each.xsl"
    ).with(new ClasspathSources(ParsingTrain.class));

    /**
     * Transform stars to tuples.
     */
    private static final XSL TUPLES = new XSLDocument(
        ParsingTrain.class.getResourceAsStream("stars-to-tuples.xsl"),
        "stars-to-tuples.xsl"
    ).with(new ClasspathSources(ParsingTrain.class));

    /**
     * Sheets in the right order.
     */
    private static final String[] SHEETS = {
        "/org/eolang/parser/errors/not-empty-atoms.xsl",
        "/org/eolang/parser/critical-errors/duplicate-names.xsl",
        "/org/eolang/parser/errors/many-free-attributes.xsl",
        "/org/eolang/parser/errors/broken-aliases.xsl",
        "/org/eolang/parser/errors/duplicate-aliases.xsl",
        "/org/eolang/parser/errors/global-nonames.xsl",
        "/org/eolang/parser/errors/same-line-names.xsl",
        "/org/eolang/parser/errors/self-naming.xsl",
        "/org/eolang/parser/cti/cti-adds-errors.xsl",
        "/org/eolang/parser/add-refs.xsl",
        "/org/eolang/parser/wrap-method-calls.xsl",
        "/org/eolang/parser/expand-qqs.xsl",
        "/org/eolang/parser/add-probes.xsl",
        "/org/eolang/parser/vars-float-up.xsl",
        "/org/eolang/parser/add-refs.xsl",
        "/org/eolang/parser/warnings/sparse-decoration.xsl",
        "/org/eolang/parser/warnings/unsorted-metas.xsl",
        "/org/eolang/parser/warnings/incorrect-architect.xsl",
        "/org/eolang/parser/warnings/incorrect-home.xsl",
        "/org/eolang/parser/warnings/incorrect-version.xsl",
        "/org/eolang/parser/expand-aliases.xsl",
        "/org/eolang/parser/resolve-aliases.xsl",
        "/org/eolang/parser/add-refs.xsl",
        "/org/eolang/parser/add-default-package.xsl",
        "/org/eolang/parser/errors/broken-refs.xsl",
        "/org/eolang/parser/errors/unknown-names.xsl",
        "/org/eolang/parser/errors/noname-attributes.xsl",
        "/org/eolang/parser/critical-errors/duplicate-names.xsl",
        "/org/eolang/parser/warnings/duplicate-metas.xsl",
        "/org/eolang/parser/warnings/mandatory-package-meta.xsl",
        "/org/eolang/parser/warnings/mandatory-home-meta.xsl",
        "/org/eolang/parser/warnings/mandatory-version-meta.xsl",
        "/org/eolang/parser/warnings/correct-package-meta.xsl",
        "/org/eolang/parser/warnings/prohibited-package.xsl",
        "/org/eolang/parser/errors/external-weak-typed-atoms.xsl",
        "/org/eolang/parser/errors/unused-aliases.xsl",
        "/org/eolang/parser/warnings/unit-test-without-phi.xsl",
        "/org/eolang/parser/explicit-data.xsl",
        "/org/eolang/parser/set-locators.xsl",
    };

    /**
     * Ctor.
     */
    @SuppressWarnings("unchecked")
    public ParsingTrain() {
        this(ParsingTrain.SHEETS);
    }

    /**
     * Ctor.
     * @param sheets Sheets
     */
    ParsingTrain(final String... sheets) {
        super(
            new TrLambda(
                new TrFast(
                    new TrLambda(
                        new TrLogged(
                            new TrJoined<>(
                                new TrDefault<>(
                                    new StEndless(
                                        new StLambda(
                                            "stars-to-tuples",
                                            ParsingTrain.TUPLES::transform
                                        )
                                    )
                                ),
                                new TrClasspath<>(sheets).back()
                            ),
                            ParsingTrain.class,
                            Level.FINEST
                        ),
                        StEoLogged::new
                    ),
                    TrFast.class,
                    500L
                ),
                shift -> new StSequence(
                    shift.uid(),
                    xml -> xml.nodes("//error[@severity='critical']").isEmpty(),
                    new StAfter(
                        shift,
                        new StLambda(
                            shift::uid,
                            (pos, xml) -> ParsingTrain.EACH.with("step", pos)
                                .with("sheet", shift.uid())
                                .transform(xml)
                        )
                    )
                )
            )
        );
    }

}
