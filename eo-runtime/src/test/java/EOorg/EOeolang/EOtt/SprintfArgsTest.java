/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOtt; // NOPMD

import org.cactoos.list.ListOf;
import org.eolang.Data;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link SprintfArgs}.
 *
 * @since 0.57.4
 */
final class SprintfArgsTest {

    @Test
    void returnsArgumentsForNumberedSubstitution() {
        final Phi tuple = Phi.Φ.take("org.eolang.tuple").copy();
        tuple.put("length", new Data.ToPhi(1));
        final String expected = "Jeff";
        tuple.put("head", new Data.ToPhi(expected));
        MatcherAssert.assertThat(
            "The sprintf args do not match with expected",
            new SprintfArgs("Hello, %s! Bye, %1$s!", 1L, tuple.take("at")).formatted(),
            Matchers.equalTo(new ListOf<>(expected, expected))
        );
    }

    @Test
    void returnsCorrectArgumentsForMixedSubstitutions() {
        final Phi tuple = Phi.Φ.take("org.eolang.tuple").copy();
        tuple.put("length", new Data.ToPhi(2));
        final Phi root = Phi.Φ.take("org.eolang.tuple").copy();
        root.put("length", new Data.ToPhi(1));
        final String first = "first";
        root.put("head", new Data.ToPhi(first));
        final String second = "second";
        tuple.put("head", new Data.ToPhi(second));
        tuple.put("tail", root);
        MatcherAssert.assertThat(
            "The sprintf args do not match with expected",
            new SprintfArgs(
                "This is the %s! This is %1$s as well! This is the %s",
                3L,
                tuple.take("at")
            ).formatted(),
            Matchers.equalTo(new ListOf<>(first, first, second))
        );
    }
}
