/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Maxmem}.
 *
 * @since 0.75.0
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
final class MaxmemTest {

    @ParameterizedTest
    @CsvSource({
        "1G, 1073741824",
        "1g, 1073741824",
        "2GB, 2147483648",
        "512M, 536870912",
        "512m, 536870912",
        "65536K, 67108864",
        "'  4M  ', 4194304",
        "1024, 1024",
        "0, 0",
        "'', 0"
    })
    void readsLimitFromProperty(final String text, final long expected) {
        MatcherAssert.assertThat(
            String.format("Value '%s' of eo.maxmem must be read as bytes, but it wasnt", text),
            Maxmem.limit(text),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void takesNoLimitFromAbsentProperty() {
        MatcherAssert.assertThat(
            "A property that is not set at all must mean no limit, but it didnt",
            Maxmem.limit(null),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void refusesToReadNonsense() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> Maxmem.limit("plenty"),
            "A value that is not a size must be refused loudly, but it wasnt"
        );
    }
}
