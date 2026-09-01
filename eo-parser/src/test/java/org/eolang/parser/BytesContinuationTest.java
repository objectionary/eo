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
 * Test case for a multi-line bytes literal.
 * @since 0.1
 */
final class BytesContinuationTest {

    @Test
    void rejectsTrailingWhitespace() {
        MatcherAssert.assertThat(
            "a bytes continuation must not strip trailing whitespace before it is validated",
            BytesContinuationTest.render("foo > main", "  CA-FE-", "  BE-BE "),
            XhtmlMatchers.hasXPath(
                "/object/errors/error[contains(text(),'trailing whitespace at end of line')]"
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
