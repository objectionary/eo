/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PhAgain}.
 *
 * @since 0.76
 */
final class PhAgainTest {

    @Test
    void signalsOnDataization() {
        Assertions.assertThrows(
            ExAgain.class,
            () -> new PhAgain(new Data.ToPhi(17L)).delta(),
            "dataizing a tail call must signal the loop, but it didnt"
        );
    }

    @Test
    void signalsOnAttributeLookup() {
        Assertions.assertThrows(
            ExAgain.class,
            () -> new PhAgain(new Data.ToPhi(3L)).take("plus"),
            "looking up an attribute of a tail call must signal the loop, but it didnt"
        );
    }

    @Test
    void signalsOnNormalization() {
        Assertions.assertThrows(
            ExAgain.class,
            () -> new PhAgain(new Data.ToPhi(1L)).normalized(),
            "normalizing a tail call must signal the loop, but it didnt"
        );
    }

    @Test
    void carriesTheCallInTheSignal() {
        final Phi next = new Data.ToPhi(29L);
        MatcherAssert.assertThat(
            "the signal must carry the very object of the tail call, but it didnt",
            Assertions.assertThrows(
                ExAgain.class,
                () -> new PhAgain(next).delta(),
                "was expected to signal"
            ).next(),
            Matchers.sameInstance(next)
        );
    }

    @Test
    void delegatesTermToTheCall() {
        MatcherAssert.assertThat(
            "the φ-term must be the one of the tail call, but it wasnt",
            new PhAgain(new PhDefault(new byte[] {(byte) 0x2A})).φTerm(),
            Matchers.equalTo("[D> 2A-]")
        );
    }

    @Test
    void recordsNoStackTrace() {
        MatcherAssert.assertThat(
            "the signal must not spend time on a stack trace, but it did",
            Assertions.assertThrows(
                ExAgain.class,
                () -> new PhAgain(new Data.ToPhi(5L)).delta(),
                "was expected to signal"
            ).getStackTrace(),
            Matchers.emptyArray()
        );
    }
}
