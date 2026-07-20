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
import org.eolang.ExFailure;
import org.eolang.PhApplication;
import org.eolang.Phi;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@code string.scanf} with a dangling {@code %} in the format.
 * @since 0.40
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class ScanfDanglingPercentTest {

    @Test
    void failsWithClearMessageOnDanglingPercent() {
        Assertions.assertThrows(
            ExFailure.class,
            () -> new Dataized(ScanfDanglingPercentTest.scanned("hello%", "hello")).take(),
            "scanf with a dangling '%' at the end of the format should fail with ExFailure, but it didn't"
        );
    }

    /**
     * Build a {@code string.scanf} call.
     * @param format Format string for {@code scanf}
     * @param read Input string for {@code scanf}
     * @return The resulting tuple
     */
    private static Phi scanned(final String format, final String read) {
        return new PhApplication(
            new PhApplication(
                Phi.Φ.take("string.scanf").copy(),
                "format", new Data.ToPhi(format)
            ),
            "read", new Data.ToPhi(read)
        );
    }
}
