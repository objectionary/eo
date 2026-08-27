/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;
import org.cactoos.Text;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Lines}.
 * @since 0.50
 */
final class LinesTest {

    @Test
    void ignoresListMutationAfterConstruction() {
        final List<Text> source = new ArrayList<>();
        source.add(new TextOf("first"));
        final Lines lines = new Lines(source);
        source.add(new TextOf("second"));
        MatcherAssert.assertThat(
            "line at an index appended to the caller's list after construction must stay empty",
            lines.line(2),
            Matchers.equalTo("")
        );
    }

    @Test
    void ignoresRemovalFromCallerListAfterConstruction() {
        final List<Text> source = new ArrayList<>();
        source.add(new TextOf("first"));
        source.add(new TextOf("second"));
        final Lines lines = new Lines(source);
        source.remove(1);
        MatcherAssert.assertThat(
            "line removed from the caller's list after construction must still be reachable",
            lines.line(2),
            Matchers.equalTo("second")
        );
    }
}
