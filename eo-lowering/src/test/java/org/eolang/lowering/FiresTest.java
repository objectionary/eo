/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.StringWriter;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Fires}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class FiresTest {

    @Test
    void choosesForkForItsLambda(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the fork λ must be served by a forking fire, but it wasnt",
            new Fires(
                new Symbols(temp.resolve("s.tsv")),
                new Boxes(temp.resolve("b.tsv")),
                new Channel(new StringWriter())
            ).at(1, "L_fork", "⟦ ⟧"),
            Matchers.instanceOf(Forking.class)
        );
    }

    @Test
    void choosesPrimitiveForListedOperation(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a listed operation must be served by a primitive fire, but it wasnt",
            new Fires(
                new Symbols(temp.resolve("s.tsv")),
                new Boxes(temp.resolve("b.tsv")),
                new Channel(new StringWriter())
            ).at(1, "L_bytes_concat", "⟦ ⟧"),
            Matchers.instanceOf(Primitive.class)
        );
    }

    @Test
    void refusesLambdaNobodyServes(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Fires(
                new Symbols(temp.resolve("s.tsv")),
                new Boxes(temp.resolve("b.tsv")),
                new Channel(new StringWriter())
            ).at(1, "L_miracle", "⟦ ⟧"),
            "a λ outside the registry must be refused, but it wasnt"
        );
    }
}
