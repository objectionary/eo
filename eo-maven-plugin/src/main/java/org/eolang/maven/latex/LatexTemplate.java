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
package org.eolang.maven.latex;

import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Latex template. Generates the LaTex template from the code
 * in LaTex notation as a standalone compilable document.
 *
 * @since 0.30
 * @todo #2067:30min We need to refactor LatexTemplate class.
 *  And to remove redundant parts in the code, like DOM variables and
 *  license header. E.g.: "&lt;listing&gt;# The MIT License (MIT)...&lt;/listing&gt;".
 */
public final class LatexTemplate {

    /**
     * The code.
     */
    private final String code;

    /**
     * Ctor.
     * @param code The code.
     */
    public LatexTemplate(final String code) {
        this.code = code;
    }

    /**
     * Generates the template from the code from
     * resources/latex-template.txt.
     * @return The generated template with the code as string.
     */
    public String asString() {
        return String.format(
            new UncheckedText(
                new TextOf(
                    new ResourceOf("org/eolang/maven/latex/latex-template.txt")
                )
            ).asString(),
            this.code
        );
    }
}
