/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.List;
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
                new Calls(Map.of("Φ.guard", List.of("fail")), Map.of()),
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
                new Calls(Map.of(), Map.of()),
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
                new Calls(Map.of("Φ.app.pair", List.of("left", "right")), Map.of()),
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
                new Calls(Map.of("Φ.box.make", List.of("k")), Map.of()),
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
                new Calls(Map.of("Φ.string", List.of("φ")), Map.of()),
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
    void followsVoidOfWhatAtomReturnsToTypeItsBracketsGive() {
        final String kind = String.format("Φ.kind%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "a read of a void of what an atom returns must copy the type its brackets give it, but it doesnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("check", "Φ.check", "status", "Φ.status", "kind", kind),
                    "Φ.check", Map.of("φ", String.format("Φ.status(Φ.string,%s)", kind)),
                    "Φ.status", Map.of("msg", "", "cde", "", "code", "Φ.status.code"),
                    kind, Map.of()
                ),
                new Calls(Map.of("Φ.status", List.of("msg", "cde")), Map.of()),
                Map.of(
                    "Φ.status.code", Map.entry("Φ.status.code.ρ", "cde"),
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "code"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "check")
                ),
                Map.of("Φ.status.code.ρ", "Φ.status", "Φ.app.φ.ρ.ρ", "Φ")
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.of(kind))
        );
    }

    @Test
    void followsVoidOfCopyMadeInEoToArgumentWrittenThere() {
        final String kind = String.format("Φ.kind%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "a read of a void of a copy made in EO must copy what the argument written there copies, but it doesnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("status", "Φ.status", kind.substring(2), kind),
                    "Φ.status", Map.of("cde", "", "code", "Φ.status.code"),
                    kind, Map.of()
                ),
                new Calls(
                    Map.of("Φ.status", List.of("cde")),
                    Map.of("Φ.app.φ.ρ", Map.of("α0", "Φ.app.φ.ρ.α0"))
                ),
                Map.of(
                    "Φ.status.code", Map.entry("Φ.status.code.ρ", "cde"),
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "code"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "status"),
                    "Φ.app.φ.ρ.α0", Map.entry("Φ.app.φ.ρ.α0.ρ", kind.substring(2))
                ),
                Map.of(
                    "Φ.status.code.ρ", "Φ.status",
                    "Φ.app.φ.ρ.ρ", "Φ",
                    "Φ.app.φ.ρ.α0.ρ", "Φ"
                )
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.of(kind))
        );
    }

    @Test
    void followsArgumentInCopyItsCallIsWrittenIn() {
        final String kind = String.format("Φ.kind%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "an argument must be read in the copy its call is written in, but it isnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("check", "Φ.check", "status", "Φ.status", "kind", kind),
                    "Φ.check", Map.of("φ", String.format("Φ.status(%s)", kind)),
                    "Φ.status",
                    Map.of("cde", "", "called", "Φ.status.called", "code", "Φ.status.code"),
                    kind, Map.of()
                ),
                new Calls(
                    Map.of("Φ.status", List.of("cde")),
                    Map.of("Φ.status.called", Map.of("α0", "Φ.status.called.α0"))
                ),
                Map.of(
                    "Φ.status.code", Map.entry("Φ.status.code.ρ", "cde"),
                    "Φ.status.called", Map.entry("Φ.status.called.ρ", "status"),
                    "Φ.status.called.α0", Map.entry("Φ.status.called.α0.ρ", "code"),
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "code"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "called"),
                    "Φ.app.φ.ρ.ρ", Map.entry("Φ.app.φ.ρ.ρ.ρ", "check")
                ),
                Map.of(
                    "Φ.status.code.ρ", "Φ.status",
                    "Φ.status.called.ρ", "Φ",
                    "Φ.status.called.α0.ρ", "Φ.status",
                    "Φ.app.φ.ρ.ρ.ρ", "Φ"
                )
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.of(kind))
        );
    }

    @Test
    void findsNothingBehindVoidThatUnknownCallerFills() {
        MatcherAssert.assertThat(
            "a read of a void filled with what an unknown caller passes must copy no formation the code fixes, but it does",
            new Origins(
                Map.of(
                    "Φ", Map.of("app", "Φ.app", "status", "Φ.status", "number", "Φ.number"),
                    "Φ.app", Map.of("f", "", "φ", "Φ.app.φ"),
                    "Φ.status", Map.of("cde", "", "code", "Φ.status.code"),
                    "Φ.number", Map.of()
                ),
                new Calls(
                    Map.of("Φ.app", List.of("f"), "Φ.status", List.of("cde")),
                    Map.of("Φ.app.φ.ρ", Map.of("α0", "Φ.app.φ.ρ.α0"))
                ),
                Map.of(
                    "Φ.status.code", Map.entry("Φ.status.code.ρ", "cde"),
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "code"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "status"),
                    "Φ.app.φ.ρ.α0", Map.entry("Φ.app.φ.ρ.α0.ρ", "f")
                ),
                Map.of(
                    "Φ.status.code.ρ", "Φ.status",
                    "Φ.app.φ.ρ.ρ", "Φ",
                    "Φ.app.φ.ρ.α0.ρ", "Φ.app"
                )
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void findsNothingBehindVoidOfCopyWithMoreVoidsThanArguments() {
        MatcherAssert.assertThat(
            "a read of a void of a copy its call leaves short of arguments must copy no formation the code fixes, but it does",
            new Origins(
                Map.of(
                    "Φ", Map.of("status", "Φ.status", "number", "Φ.number"),
                    "Φ.status", Map.of("cde", "", "msg", "", "code", "Φ.status.code"),
                    "Φ.number", Map.of()
                ),
                new Calls(
                    Map.of("Φ.status", List.of("cde", "msg")),
                    Map.of("Φ.app.φ.ρ", Map.of("α0", "Φ.app.φ.ρ.α0"))
                ),
                Map.of(
                    "Φ.status.code", Map.entry("Φ.status.code.ρ", "cde"),
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "code"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "status"),
                    "Φ.app.φ.ρ.α0", Map.entry("Φ.app.φ.ρ.α0.ρ", "number")
                ),
                Map.of(
                    "Φ.status.code.ρ", "Φ.status",
                    "Φ.app.φ.ρ.ρ", "Φ",
                    "Φ.app.φ.ρ.α0.ρ", "Φ"
                )
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void stopsOnCopiesThatApplyThemselvesInCircle() {
        MatcherAssert.assertThat(
            "copies whose attribute applies their own formation again must copy nothing, but they do",
            new Origins(
                Map.of(
                    "Φ", Map.of("count", "Φ.count"),
                    "Φ.count", Map.of("n", "", "next", "Φ.count.next")
                ),
                new Calls(
                    Map.of("Φ.count", List.of("n")),
                    Map.of("Φ.count.next.ρ", Map.of("α0", "Φ.count.next.ρ.α0"))
                ),
                Map.of(
                    "Φ.count.next", Map.entry("Φ.count.next.ρ", "next"),
                    "Φ.count.next.ρ", Map.entry("Φ.count.next.ρ.ρ", "count"),
                    "Φ.count.next.ρ.α0", Map.entry("Φ.count.next.ρ.α0.ρ", "n")
                ),
                Map.of("Φ.count.next.ρ.ρ", "Φ", "Φ.count.next.ρ.α0.ρ", "Φ.count")
            ).formation("Φ.count.next"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void stopsOnReadsThatRunInCircle() {
        MatcherAssert.assertThat(
            "reads that run back into themselves must copy nothing, but they do",
            new Origins(
                Map.of("Φ.app", Map.of("a", "Φ.app.a", "b", "Φ.app.b")),
                new Calls(Map.of(), Map.of()),
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
                new Calls(Map.of(), Map.of()),
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
