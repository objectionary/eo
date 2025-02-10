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
package org.eolang.maven.latex;

import java.util.regex.Pattern;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * Latex template. Generates the LaTex template from the code
 * in LaTex notation as a standalone compilable document.
 *
 * @since 0.30
 */
public final class LatexTemplate {
    /**
     * Pattern to extract the content starting with `+`.
     */
    private static final Pattern PATTERN = Pattern.compile("(?s)^\\s*\\+.*", Pattern.MULTILINE);

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
            this.extractedContent()
        );
    }

    /**
     * Extracts the main content from the code.
     * <p>
     * This method removes unnecessary headers (e.g., license information)
     * and retains the relevant code starting from the first line that begins with `+`.
     * If no such line is found, the entire input is returned as is.
     * @return The extracted content.
     */
    private String extractedContent() {
        return LatexTemplate.PATTERN.matcher(this.code)
            .results()
            .map(match -> match.group().trim())
            .findFirst()
            .orElse(this.code);
    }
}
