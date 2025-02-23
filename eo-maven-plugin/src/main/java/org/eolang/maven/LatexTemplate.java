/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
final class LatexTemplate {
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
    LatexTemplate(final String code) {
        this.code = code;
    }

    /**
     * Generates the template from the code from
     * resources/latex-template.txt.
     * @return The generated template with the code as string.
     */
    String asString() {
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
