/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Binding}.
 * @since 0.76.0
 */
final class BindingTest {

    @Test
    void holdsLabel() {
        MatcherAssert.assertThat(
            "the label must come back as given, but it didnt",
            new Binding("α1", new Literal("bool", "00-")).label(),
            Matchers.equalTo("α1")
        );
    }

    @Test
    void holdsValue() {
        MatcherAssert.assertThat(
            "the bound term must come back as given, but it didnt",
            new Binding("x", new Literal("bytes", "07-")).value().key(),
            Matchers.equalTo("bytes:07-")
        );
    }
}
