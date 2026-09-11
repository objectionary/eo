/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Boxes}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class BoxesTest {

    @Test
    void findsSavedBoxByLambda(@Mktmp final Path temp) throws IOException {
        final Boxes boxes = new Boxes(temp.resolve("b").resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(Arrays.asList("L_box_2", "Φ.foo.f", "number", "-", "x:number"))
            )
        );
        MatcherAssert.assertThat(
            "the saved box must be found by its λ, but it wasnt",
            boxes.at("L_box_2").locator(),
            Matchers.equalTo("Φ.foo.f")
        );
    }

    @Test
    void findsLambdaByLocator(@Mktmp final Path temp) throws IOException {
        final Boxes boxes = new Boxes(temp.resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(Arrays.asList("L_box_5", "Φ.foo.g", "bool", "bytes", ""))
            )
        );
        MatcherAssert.assertThat(
            "the λ of a boxed locator must be found, but it wasnt",
            boxes.of("Φ.foo.g"),
            Matchers.equalTo("L_box_5")
        );
    }

    @Test
    void answersBlankForUnboxedLocator(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a locator nobody boxed cannot name a λ, but it did",
            new Boxes(temp.resolve("none.tsv")).of("Φ.foo.h"),
            Matchers.is(Matchers.emptyString())
        );
    }

    @Test
    void refusesUnknownLambda(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Boxes(temp.resolve("none.tsv")).at("L_box_9"),
            "a λ without a box must be refused, but it wasnt"
        );
    }
}
