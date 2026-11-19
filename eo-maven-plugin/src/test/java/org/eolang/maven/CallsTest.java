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
    void findsArgumentWhenCallLeavesLastVoidEmpty() {
        final String arg = String.format("Φ.app.φ.α%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "a call that leaves the last void empty must still put its argument in, but it doesnt",
            new Calls(
                Map.of("Φ.padded", List.of("width", "fill", "cant-pad")),
                Map.of("Φ.app.φ", Map.of(0, arg, 1, "Φ.app.φ.α1")),
                Map.of(),
                Map.of()
            ).argument("Φ.app.φ", "Φ.padded", "width"),
            Matchers.equalTo(Optional.of(arg))
        );
    }

    @Test
    void findsNoArgumentForVoidCallLeavesEmpty() {
        MatcherAssert.assertThat(
            "a call must put nothing into a void it leaves empty, but it does",
            new Calls(
                Map.of("Φ.status", List.of("cde", "msg")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                Map.of(),
                Map.of()
            ).argument("Φ.app.φ", "Φ.status", "msg"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void findsNoArgumentWhenPositionGoesPastLastVoid() {
        MatcherAssert.assertThat(
            "a call with an argument past the last void must put nothing in, but it does",
            new Calls(
                Map.of("Φ.status", List.of("cde", "msg")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0", 2, "Φ.app.φ.α2")),
                Map.of(),
                Map.of()
            ).argument("Φ.app.φ", "Φ.status", "cde"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void findsNoArgumentWhenNameHitsFilledVoid() {
        MatcherAssert.assertThat(
            "a call that binds by name a void its positional argument fills must put nothing in",
            new Calls(
                Map.of("Φ.status", List.of("cde", "msg")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                Map.of("Φ.app.φ", Map.of("cde", "Φ.app.φ.c")),
                Map.of()
            ).argument("Φ.app.φ", "Φ.status", "cde"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void leavesOutVoidPastLastPositionalArgument() {
        MatcherAssert.assertThat(
            "a call with fewer arguments than voids must leave the last void empty, but it doesnt",
            new Calls(
                Map.of("Φ.string.at", List.of("i", "fallback")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                Map.of(),
                Map.of()
            ).leaves("Φ.app.φ", "Φ.string.at", "fallback"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotLeaveOutVoidThatGetsArgument() {
        MatcherAssert.assertThat(
            "a call must not leave empty a void its positional argument goes into, but it does",
            new Calls(
                Map.of("Φ.string.at", List.of("i", "fallback")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                Map.of(),
                Map.of()
            ).leaves("Φ.app.φ", "Φ.string.at", "i"),
            Matchers.is(false)
        );
    }

    @Test
    void doesNotLeaveOutVoidBoundByName() {
        MatcherAssert.assertThat(
            "a call must not leave empty a void it binds by name, but it does",
            new Calls(
                Map.of("Φ.string.at", List.of("i", "fallback")),
                Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                Map.of("Φ.app.φ", Map.of("fallback", "Φ.app.φ.f")),
                Map.of()
            ).leaves("Φ.app.φ", "Φ.string.at", "fallback"),
            Matchers.is(false)
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
