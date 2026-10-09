/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInfo;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Maxmem}.
 *
 * @since 0.75.0
 */
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
    @Budget("3G")
    void takesLimitFromOwnBudget(final TestInfo info) {
        MatcherAssert.assertThat(
            "The budget written on a test was not taken as its limit",
            Maxmem.budget(info.getTestMethod().orElseThrow()),
            Matchers.equalTo(3L * 1024L * 1024L * 1024L)
        );
    }

    @Test
    @Budget("0")
    void takesNoLimitFromZeroBudget(final TestInfo info) {
        MatcherAssert.assertThat(
            "A budget of zero on a test was not taken as no limit",
            Maxmem.budget(info.getTestMethod().orElseThrow()),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void takesLimitFromPropertyWithoutBudget(final TestInfo info) {
        MatcherAssert.assertThat(
            "A test with no budget of its own did not get the limit of the property",
            Maxmem.budget(info.getTestMethod().orElseThrow()),
            Matchers.equalTo(Maxmem.limit(System.getProperty("eo.maxmem")))
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
