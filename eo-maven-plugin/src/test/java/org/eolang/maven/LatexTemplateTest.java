/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
            "License header should not be present in the output",
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
