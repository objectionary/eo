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
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StAfter;
import com.yegor256.xsline.StLambda;
import com.yegor256.xsline.StSequence;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrLambda;
import com.yegor256.xsline.Train;

/**
 * Trains that adds sheet names that were processed.
 *
 * @since 0.1
 */
public final class TrStepped extends TrEnvelope {

    /**
     * Apply changes to each XML after processing.
     */
    private static final XSL EACH = new XSLDocument(
        TrStepped.class.getResourceAsStream("_each.xsl"),
        "each.xsl"
    ).with(new ClasspathSources(TrStepped.class));

    /**
     * Ctor.
     * @param train Original train
     */
    public TrStepped(final Train<Shift> train) {
        super(
            new TrLambda(
                train,
                shift -> new StSequence(
                    shift.uid(),
                    xml -> xml.nodes("//error[@severity='critical']").isEmpty(),
                    new StAfter(
                        shift,
                        new StLambda(
                            shift::uid,
                            (pos, xml) -> TrStepped.EACH
                                .with("step", pos)
                                .with("sheet", shift.uid())
                                .transform(xml)
                        )
                    )
                )
            )
        );
    }
}
