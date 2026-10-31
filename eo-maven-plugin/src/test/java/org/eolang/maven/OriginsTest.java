/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.Map;
import java.util.Optional;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Origins}.
 *
 * @since 0.76.0
 */
final class OriginsTest {

    @Test
    void findsNoFormationBehindVoid() {
        MatcherAssert.assertThat(
            "a read of a void must copy no formation the code fixes, but it does",
            new Origins(
                Map.of("Φ.guard", Map.of("fail", "", "φ", "Φ.guard.φ")),
                Map.of("Φ.guard.φ", Map.entry("Φ.guard.φ.ρ", "fail")),
                Map.of("Φ.guard.φ.ρ", "Φ.guard")
            ).formation("Φ.guard.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void followsDeclaredRhoToFormationAroundIt() {
        final String outer = String.format("Φ.oak%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "a read of a declared ρ must copy the formation it is written in, but it doesnt",
            new Origins(
                Map.of(
                    outer, Map.of("leaf", String.format("%s.leaf", outer)),
                    String.format("%s.leaf", outer), Map.of("ρ", outer)
                ),
                Map.of(
                    String.format("%s.leaf.φ.ρ", outer),
                    Map.entry(String.format("%s.leaf.φ.ρ.ρ", outer), "ρ")
                ),
                Map.of(String.format("%s.leaf.φ.ρ.ρ", outer), String.format("%s.leaf", outer))
            ).formation(String.format("%s.leaf.φ.ρ", outer)),
            Matchers.equalTo(Optional.of(outer))
        );
    }

    @Test
    void followsBoundAttributeToWhatItsBodyCopies() {
        MatcherAssert.assertThat(
            "a curried copy must copy the formation its body copies, but it doesnt",
            new Origins(
                Map.of(
                    "Φ.app", Map.of("pair", "Φ.app.pair", "half", "Φ.app.half"),
                    "Φ.app.pair", Map.of("left", "", "right", "")
                ),
                Map.of(
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "half"),
                    "Φ.app.half", Map.entry("Φ.app.half.ρ", "pair")
                ),
                Map.of("Φ.app.φ.ρ", "Φ.app", "Φ.app.half.ρ", "Φ.app")
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.of("Φ.app.pair"))
        );
    }

    @Test
    void looksForAbsentNameBehindDecoratee() {
        MatcherAssert.assertThat(
            "a name a formation does not bind must be found behind its φ, but it isnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("wrap", "Φ.wrap", "box", "Φ.box"),
                    "Φ.wrap", Map.of("φ", "Φ.wrap.φ"),
                    "Φ.box", Map.of("make", "Φ.box.make"),
                    "Φ.box.make", Map.of("k", "")
                ),
                Map.of(
                    "Φ.wrap.φ", Map.entry("Φ.wrap.φ.ρ", "box"),
                    "Φ.run", Map.entry("Φ.run.ρ", "make"),
                    "Φ.run.ρ", Map.entry("Φ.run.ρ.ρ", "wrap")
                ),
                Map.of("Φ.wrap.φ.ρ", "Φ", "Φ.run.ρ.ρ", "Φ")
            ).formation("Φ.run"),
            Matchers.equalTo(Optional.of("Φ.box.make"))
        );
    }

    @Test
    void findsNothingBehindVoidDecoratee() {
        MatcherAssert.assertThat(
            "a name read behind a void φ must copy no formation the code fixes, but it does",
            new Origins(
                Map.of(
                    "Φ", Map.of("string", "Φ.string"),
                    "Φ.string", Map.of("φ", "")
                ),
                Map.of(
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "eq"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "string")
                ),
                Map.of("Φ.app.φ.ρ.ρ", "Φ")
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void stopsOnReadsThatRunInCircle() {
        MatcherAssert.assertThat(
            "reads that run back into themselves must copy nothing, but they do",
            new Origins(
                Map.of("Φ.app", Map.of("a", "Φ.app.a", "b", "Φ.app.b")),
                Map.of(
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "a"),
                    "Φ.app.a", Map.entry("Φ.app.a.ρ", "b"),
                    "Φ.app.b", Map.entry("Φ.app.b.ρ", "a")
                ),
                Map.of("Φ.app.φ.ρ", "Φ.app", "Φ.app.a.ρ", "Φ.app", "Φ.app.b.ρ", "Φ.app")
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void stopsOnDecorateesThatRunInCircle() {
        MatcherAssert.assertThat(
            "decoratees that run back into themselves must copy nothing, but they do",
            new Origins(
                Map.of(
                    "Φ", Map.of("ping", "Φ.ping", "pong", "Φ.pong"),
                    "Φ.ping", Map.of("φ", "Φ.ping.φ"),
                    "Φ.pong", Map.of("φ", "Φ.pong.φ")
                ),
                Map.of(
                    "Φ.ping.φ", Map.entry("Φ.ping.φ.ρ", "pong"),
                    "Φ.pong.φ", Map.entry("Φ.pong.φ.ρ", "ping"),
                    "Φ.run", Map.entry("Φ.run.ρ", "zzz"),
                    "Φ.run.ρ", Map.entry("Φ.run.ρ.ρ", "ping")
                ),
                Map.of("Φ.ping.φ.ρ", "Φ", "Φ.pong.φ.ρ", "Φ", "Φ.run.ρ.ρ", "Φ")
            ).formation("Φ.run"),
            Matchers.equalTo(Optional.empty())
        );
    }
}
