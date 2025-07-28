/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOtxt; // NOPMD

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
            "The sprintf args does not match with expected",
            new SprintfArgs("Hello, %s! Bye, %1$s!", 2L, tuple.take("at")).formatted(),
            Matchers.equalTo(new ListOf<>(expected, expected))
        );
    }
}
