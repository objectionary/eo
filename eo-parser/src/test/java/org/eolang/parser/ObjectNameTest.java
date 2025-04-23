/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.ImpossibleModificationException;
import org.xembly.Xembler;

/**
 * Tests for {@link ObjectName}.
 *
 * @since 0.56.1
 */
final class ObjectNameTest {

    @Test
    void retrievesSimpleName() throws ImpossibleModificationException {
        final String expected = "foo";
        final String retrieved = new ObjectName(
            new XMLDocument(
                new Xembler(
                    new Directives().add("object").add("o").attr("name", expected)
                ).xml()
            )
        ).get();
        MatcherAssert.assertThat(
            String.format(
                "Retrieved name '%s' does not match with expected: '%s'",
                retrieved, expected
            ),
            retrieved,
            Matchers.equalTo(expected)
        );
    }
}
