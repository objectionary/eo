/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for a top-level comment block.
 * @since 0.1
 */
final class TopCommentTest {

    @Test
    void acceptsBlankBeforePlainObject() {
        MatcherAssert.assertThat(
            "the mandatory blank after a top comment must not become a sibling blank",
            TopCommentTest.render("# top doc", "", "I > foo"),
            XhtmlMatchers.hasXPaths(
                "/object/comments/comment[contains(text(),'top doc')]",
                "/object[not(errors)]"
            )
        );
    }

    /**
     * Parse rows and return their XMIR.
     * @param rows EO source rows
     * @return XMIR
     */
    private static String render(final String... rows) {
        final StringBuilder source = new StringBuilder(rows.length * 16);
        for (final String row : rows) {
            source.append(row).append((char) 10);
        }
        return new Xembler(
            new Directives().add("object").append(
                new Eo(source.toString()).directives()
            )
        ).xmlQuietly();
    }
}
