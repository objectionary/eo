/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Map;
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
    void findsSavedBoxByLambda(@Mktmp final Path temp) {
        final Boxes boxes = new Boxes(temp.resolve("b").resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(
                    Map.of(
                        "id", "L_box_2", "locator", "Φ.foo.f", "carrier", "number", "parent", "-",
                        "voids", "x:number"
                    )
                )
            )
        );
        MatcherAssert.assertThat(
            "the saved box must be found by its λ, but it wasnt",
            boxes.at("L_box_2").locator(),
            Matchers.equalTo("Φ.foo.f")
        );
    }

    @Test
    void keepsVoidsThroughTheFile(@Mktmp final Path temp) {
        final Boxes boxes = new Boxes(temp.resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(
                    Map.of(
                        "id", "L_box_7", "locator", "Φ.foo.f", "carrier", "number",
                        "parent", "-", "voids", "x:number y:object"
                    )
                )
            )
        );
        MatcherAssert.assertThat(
            "the voids must survive the trip through the file, but they didnt",
            boxes.at("L_box_7").voids(),
            Matchers.hasEntry("y", "object")
        );
    }

    @Test
    void findsLambdaByLocator(@Mktmp final Path temp) {
        final Boxes boxes = new Boxes(temp.resolve("boxes.tsv"));
        boxes.save(
            Collections.singletonList(
                new Box(
                    Map.of(
                        "id", "L_box_5", "locator", "Φ.foo.g", "carrier", "bool", "parent", "bytes",
                        "voids", ""
                    )
                )
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
