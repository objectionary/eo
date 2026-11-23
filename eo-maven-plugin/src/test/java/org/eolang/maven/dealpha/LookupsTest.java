/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven.dealpha;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;
import org.eolang.inference.Annotation;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Lookups}.
 *
 * @since 0.76.0
 */
final class LookupsTest {

    @Test
    void findsLookupItselfWhenAskedAgainWhileRunning() {
        final Lookups seen = new Lookups(new Guesses());
        final Map.Entry<String, String> key = Map.entry("Φ.grow", "lt");
        final AtomicReference<Optional<Copy>> inner = new AtomicReference<>();
        seen.once(
            key,
            () -> {
                inner.set(seen.once(key, Optional::empty));
                return Optional.empty();
            }
        );
        MatcherAssert.assertThat(
            "a lookup asked for again while it runs must find the copy that stands for itself, but it doesnt",
            inner.get().map(Copy::loops),
            Matchers.equalTo(Optional.of(true))
        );
    }

    @Test
    void remembersWhatLookupFoundOnceItEnds() {
        final Lookups seen = new Lookups(new Guesses());
        final Map.Entry<String, String> key = Map.entry("Φ.grow", "φ");
        final Copy number = new Copy(new Annotation("Φ.number"));
        seen.once(key, () -> Optional.of(number));
        MatcherAssert.assertThat(
            "a lookup asked for again after it ends must give what it found, but it doesnt",
            seen.once(key, Optional::empty),
            Matchers.equalTo(Optional.of(number))
        );
    }

    @Test
    void doesNotKeepWhatLookupFoundWhileLedBackToAnotherStillRunning() {
        final Lookups seen = new Lookups(new Guesses());
        final Map.Entry<String, String> outer = Map.entry("Φ.grow", "φ");
        final Map.Entry<String, String> inner = Map.entry("Φ.shrink", "φ");
        seen.once(
            outer,
            () -> {
                seen.once(inner, () -> seen.once(outer, Optional::empty));
                return Optional.empty();
            }
        );
        final Copy number = new Copy(new Annotation("Φ.number"));
        MatcherAssert.assertThat(
            "a lookup that was led back to another one still running must be done again once that one ends, but it isnt",
            seen.once(inner, () -> Optional.of(number)),
            Matchers.equalTo(Optional.of(number))
        );
    }
}
