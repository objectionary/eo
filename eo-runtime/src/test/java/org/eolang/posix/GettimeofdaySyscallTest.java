/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;

/**
 * Test case for {@link GettimeofdaySyscall}.
 * @since 0.74.1
 */
final class GettimeofdaySyscallTest {

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void reportsAPlausibleCurrentTimestamp() {
        final Phi result = new GettimeofdaySyscall(Phi.Φ.take("posix").copy()).make();
        final long secs = new Dataized(
            result.take("output").take("seconds")
        ).asNumber().longValue();
        final long micros = new Dataized(
            result.take("output").take("micros")
        ).asNumber().longValue();
        MatcherAssert.assertThat(
            "gettimeofday must report seconds close to the current wall-clock time, not a value corrupted by a mismatched NativeLong/Java long field width",
            (double) secs,
            Matchers.closeTo((double) (System.currentTimeMillis() / 1000L), 5.0)
        );
        MatcherAssert.assertThat(
            "gettimeofday must report a microsecond fraction below one second, not bytes read past what the native call wrote",
            micros,
            Matchers.allOf(Matchers.greaterThanOrEqualTo(0L), Matchers.lessThan(1_000_000L))
        );
    }
}
