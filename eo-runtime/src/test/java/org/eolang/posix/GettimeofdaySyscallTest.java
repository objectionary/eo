/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link GettimeofdaySyscall}.
 *
 * @since 0.74.1
 */
final class GettimeofdaySyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsSecondsCloseToCurrentWallClockTime() {
        MatcherAssert.assertThat(
            "gettimeofday must report seconds close to the current wall-clock time, not a value corrupted by a mismatched NativeLong/Java long field width",
            new Dataized(this.output().take("seconds")).asNumber().doubleValue(),
            Matchers.closeTo(System.currentTimeMillis() / 1000.0, 5.0)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsMicrosecondFractionBelowOneSecond() {
        MatcherAssert.assertThat(
            "gettimeofday must report a microsecond fraction below one second, not bytes read past what the native call wrote",
            new Dataized(this.output().take("micros")).asNumber().longValue(),
            Matchers.allOf(Matchers.greaterThanOrEqualTo(0L), Matchers.lessThan(1_000_000L))
        );
    }

    private Phi output() {
        return new GettimeofdaySyscall(Phi.Φ.take("posix").copy()).make().take("output");
    }
}
