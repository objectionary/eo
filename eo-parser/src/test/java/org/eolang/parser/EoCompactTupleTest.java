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
 * Test case for compact-tuple (*N) heads parsed by {@link Eo}.
 *
 * @since 0.1
 */
final class EoCompactTupleTest {

    @Test
    void parsesCompactTupleWithTerminatorHead() {
        MatcherAssert.assertThat(
            "a bare T head is non-chainable, but a compact-tuple line must still parse it and its children",
            EoCompactTupleTest.render(
                "[] > main",
                "  T *1 > x",
                "    \"boom\""
            ),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='main']/o[@name='x' and @base='⊥']/o[1][@base='Φ.string']",
                "/object[not(errors)]"
            )
        );
    }

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
