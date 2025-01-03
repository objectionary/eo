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
package org.eolang.maven;

import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrEnvelope;
import org.eolang.parser.TrFull;

/**
 * Train of XSL shake shifts.
 *
 * @since 0.1
 */
final class TrShaking extends TrEnvelope {
    /**
     * Ctor.
     */
    TrShaking() {
        super(
            new TrFull(
                new TrClasspath<>(
                    "/org/eolang/parser/shake/cti-adds-errors.xsl",
                    "/org/eolang/parser/shake/add-refs.xsl",
                    "/org/eolang/parser/shake/expand-qqs.xsl",
                    "/org/eolang/parser/shake/add-probes.xsl",
                    "/org/eolang/parser/shake/vars-float-up.xsl",
                    "/org/eolang/parser/shake/expand-aliases.xsl",
                    "/org/eolang/parser/shake/resolve-aliases.xsl",
                    "/org/eolang/parser/shake/add-default-package.xsl",
                    "/org/eolang/parser/shake/explicit-data.xsl",
                    "/org/eolang/parser/shake/set-locators.xsl",
                    "/org/eolang/parser/shake/clean-up.xsl",
                    "/org/eolang/parser/shake/remove-refs.xsl",
                    "/org/eolang/parser/shake/abstracts-float-up.xsl",
                    "/org/eolang/parser/shake/remove-levels.xsl",
                    "/org/eolang/parser/shake/add-refs.xsl",
                    "/org/eolang/parser/shake/fix-missed-names.xsl",
                    "/org/eolang/parser/shake/add-refs.xsl",
                    "/org/eolang/parser/shake/set-locators.xsl",
                    "/org/eolang/parser/shake/blank-xsd-schema.xsl"
                ).back()
            )
        );
    }
}
