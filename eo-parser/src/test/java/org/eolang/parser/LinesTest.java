/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Lines}.
 * @since 0.50
 */
final class LinesTest {

    @Test
    void underlinesTheOffendingLine() {
        MatcherAssert.assertThat(
            "the line at the given number is not quoted with a caret under its position",
            new Lines(List.of(new Span("привет мир", 1), new Span("второй", 2)))
                .underlined(1, 7, "боль"),
            Matchers.equalTo(String.format("[1:7] error: 'боль'%nпривет мир%n       ^"))
        );
    }

    @Test
    void keepsMessageBareWhenLineIsUnknown() {
        MatcherAssert.assertThat(
            "a number past the end of the source is not answered with the bare message",
            new Lines(List.of(new Span("", 1))).underlined(9, 2, "ошибка"),
            Matchers.equalTo("[9:2] error: 'ошибка'")
        );
    }
}
