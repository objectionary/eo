/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

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
}
