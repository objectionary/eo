/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.eolang.inference.Annotation;
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
                Map.of("Φ.guard", Map.of("fail", "Φ.guard.fail", "φ", "Φ.guard.φ")),
                new Calls(Map.of("Φ.guard", List.of("fail")), Map.of(), Map.of(), Map.of()),
                Map.of("Φ.guard.φ", Map.entry("Φ.guard.φ.ρ", "fail")),
                Map.of("Φ.guard.φ.ρ", "Φ.guard")
            ).formation("Φ.guard.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void followsVoidFilledByFormationsWithSameVoids() {
        MatcherAssert.assertThat(
            "a call on a void all callers fill with the same voids must be followed, but it isnt",
            OriginsTest.bool(List.of("left", "right"), Map.of(), Map.of())
                .formation("Φ.bool.and.φ"),
            Matchers.not(Matchers.equalTo(Optional.empty()))
        );
    }

    @Test
    void findsNoFormationBehindVoidFilledWithOtherVoids() {
        MatcherAssert.assertThat(
            "a call on a void filled with formations of other voids must not be followed, yet it is",
            OriginsTest.bool(List.of("right", "left"), Map.of(), Map.of())
                .formation("Φ.bool.and.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void findsNoFormationBehindVoidOfCopyMadeWithoutCall() {
        MatcherAssert.assertThat(
            "a void some copy leaves empty must not be followed, since ⊥ fills it, but it is",
            OriginsTest.bool(
                List.of("left", "right"), Map.of(), Map.of("Φ.app.φ", "Φ.bool")
            ).formation("Φ.bool.and.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void readsNothingOffVoidFilledWithSeveralFormations() {
        MatcherAssert.assertThat(
            "a read off a void filled with several formations must copy nothing, but it does",
            OriginsTest.bool(
                List.of("left", "right"),
                Map.of("Φ.bool.peek", Map.entry("Φ.bool.if", "φ")),
                Map.of()
            ).formation("Φ.bool.peek"),
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
                new Calls(Map.of(), Map.of(), Map.of(), Map.of()),
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
                    "Φ.app.pair", Map.of("left", "Φ.app.pair.left", "right", "Φ.app.pair.right")
                ),
                new Calls(
                    Map.of("Φ.app.pair", List.of("left", "right")), Map.of(), Map.of(), Map.of()
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
                    "Φ.box.make", Map.of("k", "Φ.box.make.k")
                ),
                new Calls(Map.of("Φ.box.make", List.of("k")), Map.of(), Map.of(), Map.of()),
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
                    "Φ.string", Map.of("φ", "Φ.string.φ")
                ),
                new Calls(Map.of("Φ.string", List.of("φ")), Map.of(), Map.of(), Map.of()),
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
    void looksForAbsentNameBehindVoidDecorateeTheEngineAnswers() {
        MatcherAssert.assertThat(
            "a name read behind a void φ the engine saw hold one formation must be found there, but it isnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("string", "Φ.string", "bytes", "Φ.bytes"),
                    "Φ.string", Map.of("φ", "Φ.string.φ"),
                    "Φ.bytes", Map.of("eq", "Φ.bytes.eq"),
                    "Φ.bytes.eq", Map.of("b", "Φ.bytes.eq.b")
                ),
                new Calls(
                    Map.of("Φ.string", List.of("φ"), "Φ.bytes.eq", List.of("b")),
                    Map.of(), Map.of(), Map.of()
                ),
                Map.of(
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "eq"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "string")
                ),
                Map.of("Φ.app.φ.ρ.ρ", "Φ"),
                Map.of("Φ.string.φ", "Φ.bytes")
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.of("Φ.bytes.eq"))
        );
    }

    @Test
    void ignoresEngineAnswerOfVoidOtherThanDecoratee() {
        MatcherAssert.assertThat(
            "a name read off a void other than φ must copy no formation, whatever the engine answers, but it does",
            new Origins(
                Map.of(
                    "Φ", Map.of("app", "Φ.app", "loud", "Φ.loud"),
                    "Φ.app", Map.of("scope", "Φ.app.scope", "φ", "Φ.app.φ"),
                    "Φ.loud", Map.of("message", "Φ.loud.message")
                ),
                new Calls(
                    Map.of("Φ.app", List.of("scope"), "Φ.loud", List.of("message")),
                    Map.of(), Map.of(), Map.of()
                ),
                Map.of("Φ.app.φ", Map.entry("Φ.app.φ.ρ", "scope")),
                Map.of("Φ.app.φ.ρ", "Φ.app"),
                Map.of("Φ.app.scope", "Φ.loud")
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void ignoresEngineAnswerOfPlaceThatIsNoVoid() {
        MatcherAssert.assertThat(
            "the answer of the engine must not name a place that is no void, but it does",
            new Origins(
                Map.of("Φ", Map.of("bytes", "Φ.bytes")),
                new Calls(Map.of(), Map.of(), Map.of(), Map.of()),
                Map.of(),
                Map.of(),
                Map.of("Φ.app.x", "Φ.bytes")
            ).formation("Φ.app.x"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void looksForAbsentNameBehindVoidDecorateeThatCallFills() {
        final String kind = String.format("Φ.kind%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "a name read behind a void φ that the call fills must be found in what fills it, but it isnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("number", "Φ.number", kind.substring(2), kind),
                    "Φ.number", Map.of("φ", "Φ.number.φ"),
                    kind, Map.of("eq", String.format("%s.eq", kind)),
                    String.format("%s.eq", kind), Map.of()
                ),
                new Calls(
                    Map.of("Φ.number", List.of("φ")),
                    Map.of("Φ.app.φ.ρ", Map.of(0, "Φ.app.φ.ρ.α0")),
                    Map.of(),
                    Map.of()
                ),
                Map.of(
                    "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "eq"),
                    "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "number"),
                    "Φ.app.φ.ρ.α0", Map.entry("Φ.app.φ.ρ.α0.ρ", kind.substring(2))
                ),
                Map.of("Φ.app.φ.ρ.ρ", "Φ", "Φ.app.φ.ρ.α0.ρ", "Φ")
            ).formation("Φ.app.φ"),
            Matchers.equalTo(Optional.of(String.format("%s.eq", kind)))
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
                    "Φ.check", Map.of(),
                    "Φ.status",
                    Map.of("msg", "Φ.status.msg", "cde", "Φ.status.cde", "code", "Φ.status.code"),
                    kind, Map.of()
                ),
                new Calls(
                    Map.of("Φ.status", List.of("msg", "cde")),
                    Map.of(),
                    Map.of(),
                    Map.of(
                        "Φ.check", new Annotation(String.format("Φ.status(Φ.string,%s)", kind))
                    )
                ),
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
                    "Φ.status", Map.of("cde", "Φ.status.cde", "code", "Φ.status.code"),
                    kind, Map.of()
                ),
                new Calls(
                    Map.of("Φ.status", List.of("cde")),
                    Map.of("Φ.app.φ.ρ", Map.of(0, "Φ.app.φ.ρ.α0")),
                    Map.of(),
                    Map.of()
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
                    "Φ.check", Map.of(),
                    "Φ.status",
                    Map.of(
                        "cde", "Φ.status.cde",
                        "called", "Φ.status.called",
                        "code", "Φ.status.code"
                    ),
                    kind, Map.of()
                ),
                new Calls(
                    Map.of("Φ.status", List.of("cde")),
                    Map.of("Φ.status.called", Map.of(0, "Φ.status.called.α0")),
                    Map.of(),
                    Map.of("Φ.check", new Annotation(String.format("Φ.status(%s)", kind)))
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
                    "Φ.app", Map.of("f", "Φ.app.f", "φ", "Φ.app.φ"),
                    "Φ.status", Map.of("cde", "Φ.status.cde", "code", "Φ.status.code"),
                    "Φ.number", Map.of()
                ),
                new Calls(
                    Map.of("Φ.app", List.of("f"), "Φ.status", List.of("cde")),
                    Map.of("Φ.app.φ.ρ", Map.of(0, "Φ.app.φ.ρ.α0")),
                    Map.of(),
                    Map.of()
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
                    "Φ.status",
                    Map.of("cde", "Φ.status.cde", "msg", "Φ.status.msg", "code", "Φ.status.code"),
                    "Φ.number", Map.of()
                ),
                new Calls(
                    Map.of("Φ.status", List.of("cde", "msg")),
                    Map.of("Φ.app.φ.ρ", Map.of(0, "Φ.app.φ.ρ.α0")),
                    Map.of(),
                    Map.of()
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
                    "Φ.count", Map.of("n", "Φ.count.n", "next", "Φ.count.next")
                ),
                new Calls(
                    Map.of("Φ.count", List.of("n")),
                    Map.of("Φ.count.next.ρ", Map.of(0, "Φ.count.next.ρ.α0")),
                    Map.of(),
                    Map.of()
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
                new Calls(Map.of(), Map.of(), Map.of(), Map.of()),
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
                new Calls(Map.of(), Map.of(), Map.of(), Map.of()),
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

    @Test
    void followsVoidFilledByReadOffResultOfCall() {
        MatcherAssert.assertThat(
            "a void filled by a read off what a call makes must copy what the read copies, but it doesnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("box", "Φ.box", "make", "Φ.make", "kind", "Φ.kind"),
                    "Φ.box", Map.of("v", "Φ.box.v", "φ", "Φ.box.φ"),
                    "Φ.make", Map.of("k", "Φ.make.k", "out", "Φ.make.out"),
                    "Φ.make.out", Map.of("go", "Φ.make.out.go"),
                    "Φ.make.out.go", Map.of(),
                    "Φ.kind", Map.of()
                ),
                new Calls(
                    Map.of("Φ.box", List.of("v"), "Φ.make", List.of("k")),
                    Map.of(
                        "Φ.app", Map.of(0, "Φ.app.α0"),
                        "Φ.app.α0.ρ", Map.of(0, "Φ.app.α0.ρ.α0")
                    ),
                    Map.of(),
                    Map.of()
                ),
                Map.of(
                    "Φ.box.φ", Map.entry("Φ.box.φ.ρ", "go"),
                    "Φ.box.φ.ρ", Map.entry("Φ.box.φ.ρ.ρ", "v"),
                    "Φ.app", Map.entry("Φ.app.ρ", "box"),
                    "Φ.app.α0", Map.entry("Φ.app.α0.ρ", "out"),
                    "Φ.app.α0.ρ", Map.entry("Φ.app.α0.ρ.ρ", "make"),
                    "Φ.app.α0.ρ.α0", Map.entry("Φ.app.α0.ρ.α0.ρ", "kind")
                ),
                Map.of(
                    "Φ.box.φ.ρ.ρ", "Φ.box",
                    "Φ.app.ρ", "Φ",
                    "Φ.app.α0.ρ.ρ", "Φ",
                    "Φ.app.α0.ρ.α0.ρ", "Φ"
                )
            ).formation("Φ.box.φ"),
            Matchers.equalTo(Optional.of("Φ.make.out.go"))
        );
    }

    @Test
    void followsVoidFilledByVoidOfCaller() {
        MatcherAssert.assertThat(
            "a void filled by a void of the caller, which a formation fills, must copy that formation, but it doesnt",
            new Origins(
                Map.of(
                    "Φ", Map.of("box", "Φ.box", "relay", "Φ.relay"),
                    "Φ.box", Map.of("v", "Φ.box.v", "φ", "Φ.box.φ"),
                    "Φ.relay", Map.of("w", "Φ.relay.w", "φ", "Φ.relay.φ"),
                    "Φ.app.α0", Map.of("go", "Φ.app.α0.go"),
                    "Φ.app.α0.go", Map.of()
                ),
                new Calls(
                    Map.of("Φ.box", List.of("v"), "Φ.relay", List.of("w")),
                    Map.of(
                        "Φ.relay.φ", Map.of(0, "Φ.relay.φ.α0"),
                        "Φ.app", Map.of(0, "Φ.app.α0")
                    ),
                    Map.of(),
                    Map.of()
                ),
                Map.of(
                    "Φ.box.φ", Map.entry("Φ.box.φ.ρ", "go"),
                    "Φ.box.φ.ρ", Map.entry("Φ.box.φ.ρ.ρ", "v"),
                    "Φ.relay.φ", Map.entry("Φ.relay.φ.ρ", "box"),
                    "Φ.relay.φ.α0", Map.entry("Φ.relay.φ.α0.ρ", "w"),
                    "Φ.app", Map.entry("Φ.app.ρ", "relay")
                ),
                Map.of(
                    "Φ.box.φ.ρ.ρ", "Φ.box",
                    "Φ.relay.φ.ρ", "Φ",
                    "Φ.relay.φ.α0.ρ", "Φ.relay",
                    "Φ.app.ρ", "Φ"
                )
            ).formation("Φ.box.φ"),
            Matchers.equalTo(Optional.of("Φ.app.α0.go"))
        );
    }

    @Test
    void followsVoidOfHelperThatPassesItToItself() {
        MatcherAssert.assertThat(
            "a void a helper passes to itself must copy what its outside caller puts there, but it doesnt",
            OriginsTest.recursive(true).formation("Φ.rec.out"),
            Matchers.equalTo(Optional.of("Φ.app.α0.go"))
        );
    }

    @Test
    void findsNothingBehindVoidOnlyItsOwnHelperFills() {
        MatcherAssert.assertThat(
            "a void only its own helper fills must copy no formation the code fixes, but it does",
            OriginsTest.recursive(false).formation("Φ.rec.out"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void findsNothingBehindVoidFilledByReadsOfFormationsWithOtherVoids() {
        MatcherAssert.assertThat(
            "a call on a void filled by formations with other voids must not be followed, yet it is",
            new Origins(
                Map.of(
                    "Φ", Map.of("box", "Φ.box", "one", "Φ.one", "two", "Φ.two"),
                    "Φ.box", Map.of("v", "Φ.box.v", "φ", "Φ.box.φ"),
                    "Φ.one", Map.of("x", "Φ.one.x"),
                    "Φ.two", Map.of("y", "Φ.two.y")
                ),
                new Calls(
                    Map.of(
                        "Φ.box", List.of("v"), "Φ.one", List.of("x"), "Φ.two", List.of("y")
                    ),
                    Map.of(
                        "Φ.box.φ", Map.of(0, "Φ.box.φ.α0"),
                        "Φ.a", Map.of(0, "Φ.a.α0"),
                        "Φ.b", Map.of(0, "Φ.b.α0")
                    ),
                    Map.of(),
                    Map.of()
                ),
                Map.of(
                    "Φ.box.φ", Map.entry("Φ.box.φ.ρ", "v"),
                    "Φ.a", Map.entry("Φ.a.ρ", "box"),
                    "Φ.a.α0", Map.entry("Φ.a.α0.ρ", "one"),
                    "Φ.b", Map.entry("Φ.b.ρ", "box"),
                    "Φ.b.α0", Map.entry("Φ.b.α0.ρ", "two")
                ),
                Map.of(
                    "Φ.box.φ.ρ", "Φ.box",
                    "Φ.a.ρ", "Φ",
                    "Φ.a.α0.ρ", "Φ",
                    "Φ.b.ρ", "Φ",
                    "Φ.b.α0.ρ", "Φ"
                )
            ).formation("Φ.box.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void findsNothingBehindVoidAtomFillsWithFormationOfOtherVoids() {
        MatcherAssert.assertThat(
            "a void an atom fills with a formation of other voids than the program puts there must copy nothing, but it does",
            OriginsTest.boxed("Φ.box(Φ.other)").formation("Φ.box.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void ignoresAtomThatReturnsTypeWithoutBrackets() {
        MatcherAssert.assertThat(
            "an atom that returns a type with no brackets must not change what the program puts into a void, but it does",
            OriginsTest.boxed("Φ.box").formation("Φ.box.φ"),
            Matchers.equalTo(Optional.of("Φ.app.α0.go"))
        );
    }

    @Test
    void followsGenericAtomWhenEveryReturnedVoidHoldsSameFormation() {
        MatcherAssert.assertThat(
            "a call on what a generic atom returns must be found where all its returned voids agree, but it isnt",
            OriginsTest.generic("number").formation("Φ.app.φ"),
            Matchers.equalTo(Optional.of("Φ.number.eq"))
        );
    }

    @Test
    void findsNothingBehindGenericAtomWhenReturnedVoidsDisagree() {
        MatcherAssert.assertThat(
            "a call on what a generic atom returns must not be found where its returned voids disagree, but it is",
            OriginsTest.generic("string").formation("Φ.app.φ"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void readsNameOffForkWhenEverySideAgrees() {
        MatcherAssert.assertThat(
            "a name read off what a fork returns must be found when both sides give the same copy, but it isnt",
            OriginsTest.forked("Φ.yes", "Φ.yes", false).formation("Φ.app"),
            Matchers.equalTo(Optional.of("Φ.yes.foo"))
        );
    }

    @Test
    void readsNothingOffForkWhenSidesDisagree() {
        MatcherAssert.assertThat(
            "a name read off what a fork returns must copy nothing when the sides disagree, but it does",
            OriginsTest.forked("Φ.yes", "Φ.no", false).formation("Φ.app"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void readsNameOffNestedFork() {
        MatcherAssert.assertThat(
            "a name read off a fork whose side is another fork must be found when all sides agree, but it isnt",
            OriginsTest.forked("Φ.yes", "Φ.yes", true).formation("Φ.app"),
            Matchers.equalTo(Optional.of("Φ.yes.foo"))
        );
    }

    @Test
    void namesArgumentsOfCallOnFork() {
        MatcherAssert.assertThat(
            "a void of a fork call must still copy its argument, but it doesnt",
            OriginsTest.forked("Φ.yes", "Φ.no", false).formation("Φ.got"),
            Matchers.equalTo(Optional.of("Φ.no"))
        );
    }

    @Test
    void followsCallOnNameReadOffVoidFilledByFormationsWithOtherVoids() {
        MatcherAssert.assertThat(
            "a call on a name that every formation filling a void binds to one formation must be followed, even when their voids differ, but it isnt",
            OriginsTest.counter(false).formation("Φ.rec.out"),
            Matchers.equalTo(Optional.of("Φ.num.eq"))
        );
    }

    @Test
    void findsNothingBehindCallOnVoidFilledByFormationsWithOtherVoids() {
        MatcherAssert.assertThat(
            "a call made right on a void filled by formations with other voids must not be followed, yet it is",
            OriginsTest.counter(false).formation("Φ.rec.go"),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void readsNothingOffVoidWhenFormationsWithOtherVoidsDisagree() {
        MatcherAssert.assertThat(
            "a name that formations with other voids filling a void bind differently must copy nothing, but it does",
            OriginsTest.counter(true).formation("Φ.rec.out"),
            Matchers.equalTo(Optional.empty())
        );
    }

    private static Origins generic(final String second) {
        return new Origins(
            Map.of(
                "Φ", Map.of("g", "Φ.g", "number", "Φ.number", "string", "Φ.string"),
                "Φ.g", Map.of("v", "Φ.g.v", "a", "Φ.g.a"),
                "Φ.number", Map.of("eq", "Φ.number.eq"),
                "Φ.number.eq", Map.of(),
                "Φ.string", Map.of("eq", "Φ.string.eq"),
                "Φ.string.eq", Map.of()
            ),
            new Calls(
                Map.of(
                    "Φ.g", List.of("v", "a"),
                    "Φ.number.eq", List.of("x"),
                    "Φ.string.eq", List.of("y")
                ),
                Map.of("Φ.app.φ.ρ", Map.of(0, "Φ.app.φ.ρ.α0", 1, "Φ.app.φ.ρ.α1")),
                Map.of(),
                Map.of("Φ.g", new Annotation("Φ.g.v")),
                Map.of("Φ.g", List.of("Φ.g.v", "Φ.g.a"))
            ),
            Map.of(
                "Φ.app.φ", Map.entry("Φ.app.φ.ρ", "eq"),
                "Φ.app.φ.ρ", Map.entry("Φ.app.φ.ρ.ρ", "g"),
                "Φ.app.φ.ρ.α0", Map.entry("Φ.app.φ.ρ.α0.ρ", "number"),
                "Φ.app.φ.ρ.α1", Map.entry("Φ.app.φ.ρ.α1.ρ", second)
            ),
            Map.of(
                "Φ.app.φ.ρ.ρ", "Φ",
                "Φ.app.φ.ρ.α0.ρ", "Φ",
                "Φ.app.φ.ρ.α1.ρ", "Φ"
            )
        );
    }

    private static Origins boxed(final String returned) {
        return new Origins(
            Map.of(
                "Φ", Map.of("box", "Φ.box", "check", "Φ.check", "other", "Φ.other"),
                "Φ.box", Map.of("v", "Φ.box.v", "φ", "Φ.box.φ"),
                "Φ.check", Map.of(),
                "Φ.other", Map.of("q", "Φ.other.q"),
                "Φ.app.α0", Map.of("go", "Φ.app.α0.go"),
                "Φ.app.α0.go", Map.of()
            ),
            new Calls(
                Map.of("Φ.box", List.of("v"), "Φ.other", List.of("q")),
                Map.of("Φ.app", Map.of(0, "Φ.app.α0")),
                Map.of(),
                Map.of("Φ.check", new Annotation(returned))
            ),
            Map.of(
                "Φ.box.φ", Map.entry("Φ.box.φ.ρ", "go"),
                "Φ.box.φ.ρ", Map.entry("Φ.box.φ.ρ.ρ", "v"),
                "Φ.app", Map.entry("Φ.app.ρ", "box")
            ),
            Map.of("Φ.box.φ.ρ.ρ", "Φ.box", "Φ.app.ρ", "Φ")
        );
    }

    private static Origins recursive(final boolean outside) {
        final Map<String, Map<Integer, String>> slots = new HashMap<>(0);
        slots.put("Φ.rec.φ", Map.of(0, "Φ.rec.φ.α0"));
        final Map<String, Map.Entry<String, String>> reads = new HashMap<>(0);
        reads.put("Φ.rec.φ", Map.entry("Φ.rec.φ.ρ", "rec"));
        reads.put("Φ.rec.φ.α0", Map.entry("Φ.rec.φ.α0.ρ", "v"));
        reads.put("Φ.rec.out", Map.entry("Φ.rec.out.ρ", "go"));
        reads.put("Φ.rec.out.ρ", Map.entry("Φ.rec.out.ρ.ρ", "v"));
        final Map<String, String> refs = new HashMap<>(0);
        refs.put("Φ.rec.φ.ρ", "Φ");
        refs.put("Φ.rec.φ.α0.ρ", "Φ.rec");
        refs.put("Φ.rec.out.ρ.ρ", "Φ.rec");
        if (outside) {
            slots.put("Φ.app", Map.of(0, "Φ.app.α0"));
            reads.put("Φ.app", Map.entry("Φ.app.ρ", "rec"));
            refs.put("Φ.app.ρ", "Φ");
        }
        return new Origins(
            Map.of(
                "Φ", Map.of("rec", "Φ.rec"),
                "Φ.rec", Map.of("v", "Φ.rec.v", "φ", "Φ.rec.φ", "out", "Φ.rec.out"),
                "Φ.app.α0", Map.of("go", "Φ.app.α0.go"),
                "Φ.app.α0.go", Map.of()
            ),
            new Calls(Map.of("Φ.rec", List.of("v")), slots, Map.of(), Map.of()),
            reads,
            refs
        );
    }

    private static Origins forked(
        final String left, final String right, final boolean nested
    ) {
        final Map<String, String> refs = new HashMap<>(0);
        refs.put("Φ.true.φ", "Φ.bool");
        refs.put("Φ.false.φ", "Φ.bool");
        refs.put("Φ.true.φ.α0.φ.ρ", "Φ.true.φ.α0");
        refs.put("Φ.false.φ.α0.φ.ρ", "Φ.false.φ.α0");
        refs.put("Φ.bool.and.φ.ρ", "Φ.bool");
        refs.put("Φ.bool.if.ρ", "Φ.bool");
        refs.put("Φ.bool.and.φ.α1", right);
        final Map<String, Map.Entry<String, String>> reads = new HashMap<>(0);
        reads.put("Φ.true.φ.α0.φ", Map.entry("Φ.true.φ.α0.φ.ρ", "left"));
        reads.put("Φ.false.φ.α0.φ", Map.entry("Φ.false.φ.α0.φ.ρ", "right"));
        reads.put("Φ.bool.and.φ", Map.entry("Φ.bool.and.φ.ρ", "if"));
        reads.put("Φ.bool.if", Map.entry("Φ.bool.if.ρ", "fork"));
        reads.put("Φ.app", Map.entry("Φ.bool.and.φ", "foo"));
        reads.put("Φ.got", Map.entry("Φ.bool.and.φ", "right"));
        final Map<String, Map<Integer, String>> slots = new HashMap<>(0);
        slots.put("Φ.true.φ", Map.of(0, "Φ.true.φ.α0"));
        slots.put("Φ.false.φ", Map.of(0, "Φ.false.φ.α0"));
        slots.put("Φ.bool.and.φ", Map.of(0, "Φ.bool.and.φ.α0", 1, "Φ.bool.and.φ.α1"));
        if (nested) {
            reads.put("Φ.bool.and.φ.α0", Map.entry("Φ.bool.and.φ.α0.ρ", "if"));
            refs.put("Φ.bool.and.φ.α0.ρ", "Φ.bool");
            refs.put("Φ.bool.and.φ.α0.α0", left);
            refs.put("Φ.bool.and.φ.α0.α1", left);
            slots.put(
                "Φ.bool.and.φ.α0",
                Map.of(0, "Φ.bool.and.φ.α0.α0", 1, "Φ.bool.and.φ.α0.α1")
            );
        } else {
            refs.put("Φ.bool.and.φ.α0", left);
        }
        return new Origins(
            Map.of(
                "Φ.bool", Map.of("fork", "Φ.bool.fork", "if", "Φ.bool.if"),
                "Φ.true.φ.α0", Map.of("φ", "Φ.true.φ.α0.φ"),
                "Φ.false.φ.α0", Map.of("φ", "Φ.false.φ.α0.φ"),
                "Φ.yes", Map.of("foo", "Φ.yes.foo"),
                "Φ.yes.foo", Map.of(),
                "Φ.no", Map.of("foo", "Φ.no.foo"),
                "Φ.no.foo", Map.of()
            ),
            new Calls(
                Map.of(
                    "Φ.bool", List.of("fork"),
                    "Φ.true.φ.α0", List.of("left", "right"),
                    "Φ.false.φ.α0", List.of("left", "right"),
                    "Φ.yes.foo", List.of("a"),
                    "Φ.no.foo", List.of("b")
                ),
                slots,
                Map.of(),
                Map.of()
            ),
            reads,
            refs
        );
    }

    private static Origins counter(final boolean own) {
        final Map<String, String> plus = new HashMap<>(
            Map.of("y", "Φ.num.plus.y", "φ", "Φ.num.plus.φ")
        );
        if (own) {
            plus.put("eq", "Φ.num.plus.eq");
        }
        return new Origins(
            Map.of(
                "Φ", Map.of("rec", "Φ.rec", "num", "Φ.num"),
                "Φ.rec", Map.of(
                    "v", "Φ.rec.v", "φ", "Φ.rec.φ", "out", "Φ.rec.out", "go", "Φ.rec.go"
                ),
                "Φ.num", Map.of("d", "Φ.num.d", "eq", "Φ.num.eq", "plus", "Φ.num.plus"),
                "Φ.num.eq", Map.of("x", "Φ.num.eq.x"),
                "Φ.num.plus", plus,
                "Φ.num.plus.eq", Map.of("z", "Φ.num.plus.eq.z")
            ),
            new Calls(
                Map.of(
                    "Φ.rec", List.of("v"), "Φ.num", List.of("d"), "Φ.num.eq", List.of("x"),
                    "Φ.num.plus", List.of("y"), "Φ.num.plus.eq", List.of("z")
                ),
                Map.of(
                    "Φ.rec.φ", Map.of(0, "Φ.rec.φ.α0"), "Φ.app", Map.of(0, "Φ.app.α0"),
                    "Φ.rec.φ.α0", Map.of(0, "Φ.rec.φ.α0.α0"),
                    "Φ.rec.out", Map.of(0, "Φ.rec.out.α0"),
                    "Φ.rec.go", Map.of(0, "Φ.rec.go.α0")
                ),
                Map.of(),
                Map.of()
            ),
            Map.of(
                "Φ.rec.φ", Map.entry("Φ.rec.φ.ρ", "rec"),
                "Φ.rec.φ.α0", Map.entry("Φ.rec.φ.α0.ρ", "plus"),
                "Φ.rec.φ.α0.ρ", Map.entry("Φ.rec.φ.α0.ρ.ρ", "v"),
                "Φ.rec.out", Map.entry("Φ.rec.out.ρ", "eq"),
                "Φ.rec.out.ρ", Map.entry("Φ.rec.out.ρ.ρ", "v"),
                "Φ.rec.go", Map.entry("Φ.rec.go.ρ", "v"),
                "Φ.app", Map.entry("Φ.app.ρ", "rec")
            ),
            Map.of(
                "Φ.rec.φ.ρ", "Φ", "Φ.rec.φ.α0.ρ.ρ", "Φ.rec", "Φ.rec.φ.α0.α0", "Φ.num",
                "Φ.rec.out.ρ.ρ", "Φ.rec", "Φ.rec.out.α0", "Φ.num", "Φ.rec.go.ρ", "Φ.rec",
                "Φ.rec.go.α0", "Φ.num", "Φ.app.ρ", "Φ", "Φ.app.α0", "Φ.num",
                "Φ.num.plus.φ", "Φ.num"
            )
        );
    }

    private static Origins bool(
        final List<String> voids, final Map<String, Map.Entry<String, String>> extra,
        final Map<String, String> more
    ) {
        final Map<String, String> refs = new HashMap<>(more);
        refs.put("Φ.true.φ", "Φ.bool");
        refs.put("Φ.false.φ", "Φ.bool");
        refs.put("Φ.bool.and.φ.ρ", "Φ.bool");
        refs.put("Φ.bool.if.ρ", "Φ.bool");
        final Map<String, Map.Entry<String, String>> reads = new HashMap<>(extra);
        reads.put("Φ.bool.and.φ", Map.entry("Φ.bool.and.φ.ρ", "if"));
        reads.put("Φ.bool.if", Map.entry("Φ.bool.if.ρ", "fork"));
        return new Origins(
            Map.of(
                "Φ.bool", Map.of("fork", "Φ.bool.fork", "if", "Φ.bool.if"),
                "Φ.true.φ.α0", Map.of("φ", "Φ.yes"),
                "Φ.false.φ.α0", Map.of("φ", "Φ.no"),
                "Φ.yes", Map.of(),
                "Φ.no", Map.of()
            ),
            new Calls(
                Map.of(
                    "Φ.bool", List.of("fork"),
                    "Φ.true.φ.α0", List.of("left", "right"),
                    "Φ.false.φ.α0", voids,
                    "Φ.yes", List.of("a"),
                    "Φ.no", List.of("b")
                ),
                Map.of(
                    "Φ.true.φ", Map.of(0, "Φ.true.φ.α0"),
                    "Φ.false.φ", Map.of(0, "Φ.false.φ.α0"),
                    "Φ.bool.and.φ", Map.of(0, "Φ.bool.and.φ.α0", 1, "Φ.bool.and.φ.α1")
                ),
                Map.of(),
                Map.of()
            ),
            reads,
            refs
        );
    }
}
