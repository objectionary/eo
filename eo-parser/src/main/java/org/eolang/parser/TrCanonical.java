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

import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.StEndless;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrEnvelope;
import com.yegor256.xsline.TrJoined;

/**
 * Train of XSL shifts that turn XMIR into canonical one.
 *
 * @since 0.48
 */
final class TrCanonical extends TrEnvelope {
    /**
     * Ctor.
     */
    TrCanonical() {
        super(
            new TrFull(
                new TrJoined<>(
                    new TrClasspath<>(
                        "/org/eolang/parser/parse/move-voids-up.xsl",
                        "/org/eolang/parser/parse/validate-before-stars.xsl",
                        "/org/eolang/parser/parse/resolve-before-star.xsl"
                    ).back(),
                    new TrDefault<>(
                        new StEndless(
                            new StClasspath(
                                "/org/eolang/parser/parse/stars-to-tuples.xsl"
                            )
                        )
                    ),
                    new TrClasspath<>(
                        "/org/eolang/parser/parse/wrap-method-calls.xsl",
                        "/org/eolang/parser/parse/const-to-dataized.xsl"
                    ).back()
                )
            )
        );
    }
}
