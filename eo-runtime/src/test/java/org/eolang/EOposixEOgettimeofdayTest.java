/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Collection;
import java.util.HashSet;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link EOposix$EOgettimeofday}.
 *
 * @since 0.77.0
 */
final class EOposixEOgettimeofdayTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsSecondsCloseToCurrentWallClockTime() {
        MatcherAssert.assertThat(
            "gettimeofday must report seconds close to the current wall-clock time, not a value corrupted by a mismatched NativeLong/Java long field width",
            new Dataized(
                new EOposix$EOgettimeofday().take("seconds")
            ).asNumber(),
            Matchers.closeTo(System.currentTimeMillis() / 1000.0, 5.0)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsMicrosecondFractionBelowOneSecond() {
        MatcherAssert.assertThat(
            "gettimeofday must report a microsecond fraction below one second, not bytes read past what the native call wrote",
            new Dataized(
                new EOposix$EOgettimeofday().take("micros")
            ).asNumber().longValue(),
            Matchers.allOf(Matchers.greaterThanOrEqualTo(0L), Matchers.lessThan(1_000_000L))
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void readsTheClockOnceForTheWholeAnswer() {
        final Phi answer = new EOposix$EOgettimeofday().take("called");
        final Collection<Number> reported = new HashSet<>(1);
        for (int read = 0; read < 16; ++read) {
            reported.add(new Dataized(answer.take("micros")).asNumber());
        }
        MatcherAssert.assertThat(
            "every read of one answer must report the same microsecond, or seconds and micros come from two readings of the clock and name a time that never was",
            reported,
            Matchers.hasSize(1)
        );
    }
}
