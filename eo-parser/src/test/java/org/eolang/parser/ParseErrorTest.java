/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link ParseError}.
 * @since 0.1
 */
final class ParseErrorTest {

    @Test
    void retainsLineFromCtor() {
        MatcherAssert.assertThat(
            "line must round-trip the ctor argument so reporters can quote source location",
            new ParseError(7, 3, "boom").line(),
            Matchers.equalTo(7)
        );
    }

    @Test
    void retainsPosFromCtor() {
        MatcherAssert.assertThat(
            "pos must round-trip the ctor argument with 0-indexed semantics",
            new ParseError(1, 12, "boom").pos(),
            Matchers.equalTo(12)
        );
    }

    @Test
    void exposesMessageThroughGetMessage() {
        MatcherAssert.assertThat(
            "the canonical message text must be readable via getMessage()",
            new ParseError(1, 0, "unexpected odd indent").getMessage(),
            Matchers.equalTo("unexpected odd indent")
        );
    }
}
