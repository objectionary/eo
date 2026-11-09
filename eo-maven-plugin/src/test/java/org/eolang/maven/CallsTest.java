/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.eolang.inference.Annotation;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Calls}.
 *
 * @since 0.76.0
 */
final class CallsTest {

    @Test
    void findsArgumentAtPositionOfVoid() {
        final String arg = "Φ.app.φ.α1";
        MatcherAssert.assertThat(
            "a positional argument must go into the void at its position, but it doesnt",
            new Calls(
                Map.of("Φ.status", List.of("cde", "msg")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0", 1, arg)),
                Map.of(),
                Map.of()
            ).argument("Φ.app.φ", "Φ.status", "msg"),
            Matchers.equalTo(Optional.of(arg))
        );
    }

    @Test
    void findsArgumentBoundToVoidByName() {
        final String arg = String.format("Φ.app.φ.x%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "an argument bound by name must go into the void it names, but it doesnt",
            new Calls(
                Map.of("Φ.status", List.of("cde", "msg")),
                Map.of(),
                Map.of("Φ.app.φ", Map.of("msg", "Φ.app.φ.y", "cde", arg)),
                Map.of()
            ).argument("Φ.app.φ", "Φ.status", "cde"),
            Matchers.equalTo(Optional.of(arg))
        );
    }

    @Test
    void findsNoArgumentWhenCallLeavesVoidVacant() {
        MatcherAssert.assertThat(
            "a call with fewer arguments than voids must put nothing the code fixes, but it does",
            new Calls(
                Map.of("Φ.status", List.of("cde", "msg")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                Map.of(),
                Map.of()
            ).argument("Φ.app.φ", "Φ.status", "cde"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void findsTypeBracketsGiveVoid() {
        final String kind = String.format("Φ.kind%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "a void must hold the type the brackets give at its position, but it doesnt",
            new Calls(
                Map.of("Φ.status", List.of("cde", "msg")), Map.of(), Map.of(), Map.of()
            ).member(new Annotation(String.format("Φ.status(Φ.number,%s)", kind)), "msg"),
            Matchers.equalTo(Optional.of(new Annotation(kind)))
        );
    }
}
