/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_string; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@code string.scanf} with signed integer inputs.
 * @since 0.57.4
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class ScanfNegativeIntegerTest {

    @Test
    void parsesNegativeIntegerWithDecimalFormat() {
        MatcherAssert.assertThat(
            "scanf with \"%d\" should preserve the leading minus sign for negative integers",
            new Dataized(ScanfNegativeIntegerTest.first("%d", "-42")).asNumber().longValue(),
            Matchers.equalTo(-42L)
        );
    }

    @Test
    void parsesPositiveIntegerWithLeadingPlusSign() {
        MatcherAssert.assertThat(
            "scanf with \"%d\" should accept an optional leading plus sign",
            new Dataized(ScanfNegativeIntegerTest.first("%d", "+42")).asNumber().longValue(),
            Matchers.equalTo(42L)
        );
    }

    @Test
    void parsesPlainPositiveIntegerWithDecimalFormat() {
        MatcherAssert.assertThat(
            "scanf with \"%d\" should still parse plain unsigned integers",
            new Dataized(ScanfNegativeIntegerTest.first("%d", "42")).asNumber().longValue(),
            Matchers.equalTo(42L)
        );
    }

    /**
     * Build a {@code string.scanf} call and return the first parsed item.
     * @param format Format string for {@code scanf}
     * @param read Input string for {@code scanf}
     * @return Phi at index 0 of the resulting tuple
     */
    private static Phi first(final String format, final String read) {
        final Phi item = new PhApplication(
            new PhApplication(
                Phi.Φ.take("string.scanf").copy(),
                "format", new Data.ToPhi(format)
            ),
            "read", new Data.ToPhi(read)
        ).take("at").copy();
        item.put(0, new Data.ToPhi(0L));
        return item;
    }
}
