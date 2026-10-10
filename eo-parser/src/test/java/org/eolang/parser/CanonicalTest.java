/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Canonical}.
 *
 * @since 0.60
 */
final class CanonicalTest {

    @Test
    void resolvesLocalNamesThroughSingleArgumentFunctions() throws Exception {
        MatcherAssert.assertThat(
            "resolve-local-names declares a multi-argument function, which Saxon 13 misbinds across threads",
            new XMLDocument(
                new TextOf(
                    new ResourceOf("org/eolang/parser/parse/resolve-local-names.xsl")
                ).asString()
            ).nodes("//*[local-name()='function'][count(*[local-name()='param']) > 1]"),
            Matchers.empty()
        );
    }
}
