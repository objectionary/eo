/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import org.cactoos.io.InputOf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test for error recovery in parser.
 *
 * @since 0.1
 */
final class ErrorRecoveryTest {

    /**
     * Test that parser recovers from malformed attribute syntax.
     * @throws Exception If test fails
     */
    @Test
    void testErrorRecovery() throws Exception {
        final String code = String.join(
            "\n",
            "# Example.",
            "[] > example",
            "  [x] +++ bad",
            "    one",
            "      two",
            "  [] > good",
            "    one",
            "      two"
        );
        final XML xml = new EoSyntax(new InputOf(code)).parsed();
        final String listing = xml.xpath("/object/listing/text()").get(0);
        Assertions.assertTrue(
            listing.contains("good"),
            "Should preserve full listing including good object"
        );
    }

    /**
     * Test simple error recovery case.
     * @throws Exception If test fails
     */
    @Test
    void testSimpleErrorRecovery() throws Exception {
        final String code = String.join(
            "\n",
            "# Simple test.",
            "[] > obj",
            "  [x] +++ bad",
            "  [] > good"
        );
        final XML xml = new EoSyntax(new InputOf(code)).parsed();
        final String listing = xml.xpath("/object/listing/text()").get(0);
        Assertions.assertTrue(
            listing.contains("good"),
            "Should preserve full listing in simple case"
        );
    }
}
