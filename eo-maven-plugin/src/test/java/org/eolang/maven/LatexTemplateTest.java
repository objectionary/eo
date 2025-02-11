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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link LatexTemplate}.
 *
 * @since 0.30
 */
final class LatexTemplateTest {

    /**
     * The definition of the documentclass.
     */
    private static final String DOCUMENT_CLASS = "\\documentclass{article}";

    /**
     * The definition of the usepackage.
     */
    private static final String USE_PACKAGE = "\\usepackage{ffcode}";

    /**
     * The beginning of the document.
     */
    private static final String BEGIN_DOCUMENT = "\\begin{document}";

    /**
     * The beginning of the document.
     */
    private static final String BEGIN_FF_CODE = "\\begin{ffcode}";

    /**
     * The ending of the document.
     */
    private static final String END_FF_CODE = "\\end{ffcode}";

    /**
     * The ending of the document.
     */
    private static final String END_DOCUMENT = "\\end{document}";

    /**
     * Check the full template.
     */
    @Test
    void generatesFullTemplate() {
        MatcherAssert.assertThat(
            "LatexTemplate.asString() gives incorrect template",
            new LatexTemplate(
                "+package f\n[args] > main\n  stdout \"Hello!\""
            ).asString(),
            Matchers.stringContainsInOrder(
                LatexTemplateTest.DOCUMENT_CLASS,
                LatexTemplateTest.USE_PACKAGE,
                LatexTemplateTest.BEGIN_DOCUMENT,
                LatexTemplateTest.BEGIN_FF_CODE,
                "+package f",
                "[args] > main",
                "  stdout \"Hello!\"",
                LatexTemplateTest.END_FF_CODE,
                LatexTemplateTest.END_DOCUMENT
            )
        );
    }

    @Test
    void removesRedundantPartsInCode() {
        final String output = new LatexTemplate(
            LatexTemplateTest.input(
                "# The MIT License (MIT)\n# Copyright (c)",
                "+architect yegor256@gmail.com",
                "+package f\n [args] > main",
                "  stdout \"Hello!\""
            )
        ).asString();
        MatcherAssert.assertThat(
            "License header shoud not be present in the output",
            output,
            Matchers.not(Matchers.containsString("The MIT License"))
        );
        MatcherAssert.assertThat(
            "Code listing should be present in the output",
            output,
            Matchers.stringContainsInOrder(
                LatexTemplateTest.DOCUMENT_CLASS,
                LatexTemplateTest.USE_PACKAGE,
                LatexTemplateTest.BEGIN_DOCUMENT,
                LatexTemplateTest.BEGIN_FF_CODE,
                "+architect yegor256@gmail.com",
                "+package f",
                "[args] > main",
                "  stdout \"Hello!\"",
                LatexTemplateTest.END_FF_CODE,
                LatexTemplateTest.END_DOCUMENT
            )
        );
    }

    private static String input(final String... inputs) {
        return String.join("\n", inputs);
    }
}
