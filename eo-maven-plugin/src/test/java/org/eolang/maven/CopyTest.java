/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Copy}.
 *
 * @since 0.76.0
 */
final class CopyTest {

    @Test
    void readsArgumentOfVoidInCopyItsCallIsWrittenIn() {
        final String host = String.format("Φ.host%d(Φ.number)", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "the argument in a void must be read in the copy its call is written in, but it isnt",
            new Copy("Φ.status").applied("Φ.app.φ", new Copy(host)).filler(
                "cde",
                new Calls(
                    Map.of("Φ.status", List.of("cde")),
                    Map.of("Φ.app.φ", Map.of("α0", "Φ.app.φ.α0"))
                )
            ).map(spot -> String.join(" ", spot.getKey(), spot.getValue().toString())).get(),
            Matchers.equalTo(String.join(" ", "Φ.app.φ.α0", host))
        );
    }

    @Test
    void keepsOnlyFormationOfHostMadeByCall() {
        MatcherAssert.assertThat(
            "a copy made by a call must be kept as its formation alone when another call is written in it, but it isnt",
            new Copy("Φ.pair").applied(
                "Φ.app.φ",
                new Copy("Φ.box").applied("Φ.app.box", new Copy("Φ.app"))
            ).filler(
                "left",
                new Calls(
                    Map.of("Φ.pair", List.of("left")),
                    Map.of("Φ.app.φ", Map.of("α0", "Φ.app.φ.α0"))
                )
            ).get().getValue().toString(),
            Matchers.equalTo("Φ.box")
        );
    }
}
