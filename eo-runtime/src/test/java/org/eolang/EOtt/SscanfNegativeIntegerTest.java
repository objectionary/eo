/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EOtt; // NOPMD

import org.eolang.Data;
import org.eolang.Dataized;
import org.eolang.PhCopy;
import org.eolang.PhWith;
import org.eolang.Phi;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@code tt.sscanf} with signed integer inputs.
 * @since 0.57.4
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class SscanfNegativeIntegerTest {

    @Test
    void parsesNegativeIntegerWithDecimalFormat() {
        MatcherAssert.assertThat(
            "sscanf with \"%d\" should preserve the leading minus sign for negative integers",
            new Dataized(SscanfNegativeIntegerTest.first("%d", "-42")).asNumber().longValue(),
            Matchers.equalTo(-42L)
        );
    }

    @Test
    void parsesPositiveIntegerWithLeadingPlusSign() {
        MatcherAssert.assertThat(
            "sscanf with \"%d\" should accept an optional leading plus sign",
            new Dataized(SscanfNegativeIntegerTest.first("%d", "+42")).asNumber().longValue(),
            Matchers.equalTo(42L)
        );
    }

    @Test
    void parsesPlainPositiveIntegerWithDecimalFormat() {
        MatcherAssert.assertThat(
            "sscanf with \"%d\" should still parse plain unsigned integers",
            new Dataized(SscanfNegativeIntegerTest.first("%d", "42")).asNumber().longValue(),
            Matchers.equalTo(42L)
        );
    }

    /**
     * Build a {@code tt.sscanf} call and return the first parsed item.
     * @param format Format string for {@code sscanf}
     * @param read Input string for {@code sscanf}
     * @return Phi at index 0 of the resulting tuple
     */
    private static Phi first(final String format, final String read) {
        final Phi item = new PhWith(
            new PhWith(
                new PhCopy(Phi.Φ.take("tt.sscanf")),
                "format", new Data.ToPhi(format)
            ),
            "read", new Data.ToPhi(read)
        ).take("at").copy();
        item.put(0, new Data.ToPhi(0L));
        return item;
    }
}
