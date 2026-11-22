/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiFunction;
import org.eolang.inference.Annotation;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Fillers}.
 *
 * <p>Every case is about {@code [x cant] > safe}, whose void {@code cant}
 * one call leaves empty, as {@code safe 5} does, while others fill it. The
 * call at {@code Φ.app.a} passes one argument, into {@code x}, and so
 * leaves {@code cant} empty. Any other call passes two, and its second
 * argument, the one in {@code cant}, is a terminator at {@code Φ.app.t}, a
 * copy of {@code fix} at {@code Φ.app.f}, and unknown anywhere else.</p>
 *
 * <p>Other cases are about {@code [a b] > pair}, copied by one call at
 * {@code Φ.app.c}, whose argument in {@code a} asks twice what {@code b}
 * holds, while the argument in {@code b} leans on {@code a}.</p>
 *
 * @since 0.76.0
 */
final class FillersTest {

    @Test
    void holdsTerminatorWhenOneCallPassesItAndAnotherLeavesVoidEmpty() {
        MatcherAssert.assertThat(
            "a void left empty must hold the terminator when a call puts the terminator there, but it doesnt",
            FillersTest.fillers(
                Set.of("Φ.app.a", "Φ.app.t"), Set.of()
            ).held("Φ.safe.cant", Map.entry("Φ.safe", "cant")).getKey(),
            Matchers.equalTo(Optional.of(Copy.terminator()))
        );
    }

    @Test
    void keepsVoidUnknownWhenNoCallPassesTerminator() {
        MatcherAssert.assertThat(
            "a void left empty must stay unknown when no call puts the terminator there, but it doesnt",
            FillersTest.fillers(
                Set.of("Φ.app.a", "Φ.app.f"), Set.of()
            ).held("Φ.safe.cant", Map.entry("Φ.safe", "cant")).getKey(),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void namesVoidThatLacksCallWithTerminator() {
        final Fillers fillers = FillersTest.fillers(
            Set.of("Φ.app.a", "Φ.app.f"), Set.of()
        );
        fillers.held("Φ.safe.cant", Map.entry("Φ.safe", "cant"));
        MatcherAssert.assertThat(
            "a void that a test with the terminator would make known must be named, but it isnt",
            fillers.vacant(),
            Matchers.contains(Map.entry("Φ.safe", "cant"))
        );
    }

    @Test
    void holdsWhatOtherCallsPutWhenBareCopyLeavesVoidEmpty() {
        MatcherAssert.assertThat(
            "a void a bare copy leaves empty must hold what the other calls put there, when one of them puts the terminator there, but it doesnt",
            FillersTest.fillers(
                Set.of("Φ.app.t", "Φ.app.f"),
                Set.of("Φ.safe")
            ).held("Φ.safe.cant", Map.entry("Φ.safe", "cant")).getKey(),
            Matchers.equalTo(Optional.of(new Copy(List.of("Φ.fix")).ending(true)))
        );
    }

    @Test
    void keepsVoidUnknownWhenSomeArgumentIsUnknown() {
        MatcherAssert.assertThat(
            "a void must stay unknown when a call puts an unknown argument there, even with the terminator from another call, but it doesnt",
            FillersTest.fillers(
                Set.of("Φ.app.t", "Φ.app.u"), Set.of()
            ).held("Φ.safe.cant", Map.entry("Φ.safe", "cant")).getKey(),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void keepsVoidUnknownWhenNothingFillsIt() {
        MatcherAssert.assertThat(
            "a void that no call fills must stay unknown, but it doesnt",
            FillersTest.fillers(Set.of(), Set.of()).held(
                "Φ.safe.cant", Map.entry("Φ.safe", "cant")
            ).getKey(),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void givesBackWhatVoidLeaningOnOpenVoidHeldInSameSituation() {
        final AtomicInteger shaped = new AtomicInteger();
        FillersTest.chain(shaped).held("Φ.pair.a", Map.entry("Φ.pair", "a"));
        MatcherAssert.assertThat(
            "a void that leans on an open void must be worked out once per guess of that void, not once per lookup, but it isnt",
            shaped.get(),
            Matchers.equalTo(4)
        );
    }

    @Test
    void givesBackWhatVoidLeaningOnOpenVoidHeldAsNotSettled() {
        final List<Boolean> settled = new ArrayList<>(2);
        final AtomicReference<Fillers> self = new AtomicReference<>();
        final Fillers fillers = FillersTest.pair(
            (place, seen) -> {
                final Optional<Copy> found;
                if ("Φ.app.c.α0".equals(place)) {
                    settled.add(self.get().held("Φ.pair.b", Map.entry("Φ.pair", "b")).getValue());
                    settled.add(self.get().held("Φ.pair.b", Map.entry("Φ.pair", "b")).getValue());
                    found = Optional.of(new Copy(new Annotation("Φ.fix")));
                } else {
                    self.get().hit("Φ.pair.a");
                    found = Optional.of(new Copy(new Annotation("Φ.fix")));
                }
                return found;
            }
        );
        self.set(fillers);
        fillers.held("Φ.pair.a", Map.entry("Φ.pair", "a"));
        MatcherAssert.assertThat(
            "what a void that leans on an open void holds must never be remembered for good, even when given back, but it is",
            settled,
            Matchers.everyItem(Matchers.equalTo(false))
        );
    }

    @Test
    void worksVoidOutAgainWhenVoidItOpenedIsOpenNow() {
        final AtomicInteger shaped = new AtomicInteger();
        final AtomicReference<Fillers> self = new AtomicReference<>();
        final Map<String, List<String>> asks = Map.of(
            "Φ.app.c.α0", List.of("b", "c"),
            "Φ.app.c.α1", List.of("a", "c"),
            "Φ.app.c.α2", List.of("a", "b")
        );
        final Fillers fillers = new Fillers(
            Map.of(
                "Φ.tri", Map.of("a", "Φ.tri.a", "b", "Φ.tri.b", "c", "Φ.tri.c"),
                "Φ.fix", Map.of()
            ),
            new Calls(
                Map.of("Φ.tri", List.of("a", "b", "c")),
                Map.of("Φ.app.c", Map.of(0, "Φ.app.c.α0", 1, "Φ.app.c.α1", 2, "Φ.app.c.α2")),
                Collections.emptyMap(), Collections.emptyMap()
            ),
            Map.of("Φ.tri", List.of("Φ.app.c")),
            Set.of(),
            (place, seen) -> {
                if ("Φ.app.c.α1".equals(place)) {
                    shaped.incrementAndGet();
                }
                for (final String name : asks.get(place)) {
                    final String hole = String.format("Φ.tri.%s", name);
                    if (self.get().open(hole)) {
                        self.get().hit(hole);
                    } else {
                        self.get().held(hole, Map.entry("Φ.tri", name));
                    }
                }
                return Optional.of(new Copy(new Annotation("Φ.fix")));
            }
        );
        self.set(fillers);
        fillers.held("Φ.tri.a", Map.entry("Φ.tri", "a"));
        MatcherAssert.assertThat(
            "a void must be worked out again when a void it opened itself is open now, since it would read the guess of that void instead, but it isnt",
            shaped.get(),
            Matchers.equalTo(12)
        );
    }

    private static Fillers chain(final AtomicInteger shaped) {
        final AtomicReference<Fillers> self = new AtomicReference<>();
        final Fillers fillers = FillersTest.pair(
            (place, seen) -> {
                final Optional<Copy> found;
                if ("Φ.app.c.α0".equals(place)) {
                    self.get().held("Φ.pair.b", Map.entry("Φ.pair", "b"));
                    found = self.get().held("Φ.pair.b", Map.entry("Φ.pair", "b")).getKey();
                } else {
                    shaped.incrementAndGet();
                    self.get().hit("Φ.pair.a");
                    found = Optional.of(new Copy(new Annotation("Φ.fix")));
                }
                return found;
            }
        );
        self.set(fillers);
        return fillers;
    }

    private static Fillers pair(final BiFunction<String, Lookups, Optional<Copy>> lookup) {
        return new Fillers(
            Map.of(
                "Φ.pair", Map.of("a", "Φ.pair.a", "b", "Φ.pair.b"),
                "Φ.fix", Map.of()
            ),
            new Calls(
                Map.of("Φ.pair", List.of("a", "b")),
                Map.of("Φ.app.c", Map.of(0, "Φ.app.c.α0", 1, "Φ.app.c.α1")),
                Collections.emptyMap(), Collections.emptyMap()
            ),
            Map.of("Φ.pair", List.of("Φ.app.c")),
            Set.of(),
            lookup
        );
    }

    private static Fillers fillers(final Set<String> calls, final Set<String> bare) {
        final Map<String, Map<Integer, String>> positional = new HashMap<>(0);
        for (final String call : calls) {
            final Map<Integer, String> args = new HashMap<>(0);
            args.put(0, String.format("%s.α0", call));
            if (!"Φ.app.a".equals(call)) {
                args.put(1, String.format("%s.α1", call));
            }
            positional.put(call, args);
        }
        return new Fillers(
            Map.of(
                "Φ.safe", Map.of("x", "Φ.safe.x", "cant", "Φ.safe.cant"),
                "Φ.fix", Map.of()
            ),
            new Calls(
                Map.of("Φ.safe", List.of("x", "cant")),
                positional, Collections.emptyMap(), Collections.emptyMap()
            ),
            Map.of("Φ.safe", new ArrayList<>(new TreeSet<>(calls))),
            bare,
            (place, seen) -> {
                final Optional<Copy> found;
                if ("Φ.app.t.α1".equals(place)) {
                    found = Optional.of(Copy.terminator());
                } else if ("Φ.app.f.α1".equals(place)) {
                    found = Optional.of(new Copy(new Annotation("Φ.fix")));
                } else {
                    found = Optional.empty();
                }
                return found;
            }
        );
    }
}
