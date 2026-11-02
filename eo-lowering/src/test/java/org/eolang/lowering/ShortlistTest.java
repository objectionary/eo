/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.security.SecureRandom;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests of the class {@link Shortlist}.
 *
 * @since 0.64.0
 */
final class ShortlistTest {

    @Test
    void showsEveryNameOfAShortList() {
        final int max = new SecureRandom().nextInt(5) + 3;
        final List<String> names = ShortlistTest.names(new SecureRandom().nextInt(max) + 1);
        MatcherAssert.assertThat(
            "a list not longer than the limit must be shown whole, but it isnt",
            new Shortlist(names, max).asString(),
            Matchers.equalTo(String.join(", ", names))
        );
    }

    @Test
    void countsTheNamesLeftOutOfALongList() {
        final int max = new SecureRandom().nextInt(5) + 3;
        final int more = new SecureRandom().nextInt(40) + 1;
        final List<String> names = ShortlistTest.names(max + more);
        MatcherAssert.assertThat(
            "a list longer than the limit must say how many names it leaves out, but it doesnt",
            new Shortlist(names, max).asString(),
            Matchers.equalTo(
                String.format("%s, and %d more", String.join(", ", names.subList(0, max)), more)
            )
        );
    }

    private static List<String> names(final int count) {
        return IntStream.range(0, count)
            .mapToObj(idx -> String.format("vy%d.k%d", idx, new SecureRandom().nextInt(1000)))
            .collect(Collectors.toList());
    }
}
