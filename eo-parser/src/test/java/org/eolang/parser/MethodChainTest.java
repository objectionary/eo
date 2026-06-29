/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link MethodChain}.
 * @since 0.1
 */
final class MethodChainTest {

    @Test
    void retainsMethodName() {
        MatcherAssert.assertThat(
            "name() must round-trip the method identifier without the leading dot",
            new MethodChain("bar", 3, 7, false).name(),
            Matchers.equalTo("bar")
        );
    }

    @Test
    void retainsDotColumnForPosAttribute() {
        MatcherAssert.assertThat(
            "dot() must round-trip the column where the leading dot sits per R-9.1.3",
            new MethodChain("bar", 3, 7, false).dot(),
            Matchers.equalTo(3)
        );
    }

    @Test
    void retainsEndIndex() {
        MatcherAssert.assertThat(
            "end() must round-trip the cursor-advance position past the link",
            new MethodChain("bar", 3, 7, false).end(),
            Matchers.equalTo(7)
        );
    }

    @Test
    void retainsFragileFlag() {
        MatcherAssert.assertThat(
            "fragile() must round-trip the `?.` dispatch marker",
            new MethodChain("read", 3, 9, true).fragile(),
            Matchers.equalTo(true)
        );
    }
}
