/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang.sys;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Inaddr}.
 *
 * @since 0.77.0
 */
final class InaddrTest {

    @Test
    void widensTheNumberToUnsigned() {
        MatcherAssert.assertThat(
            "inet_addr answers with an in_addr_t, which is unsigned, so an address whose first part is 128 or above must not reach EO as a negative number",
            new Inaddr("10.0.0.200", -939_524_086).it(),
            Matchers.equalTo(3_355_443_210L)
        );
    }

    @Test
    void reportsFailureOnTextItCannotConvert() {
        MatcherAssert.assertThat(
            "INADDR_NONE for text that is no address at all must be reported as a failure, or the caller reaches a host nobody named",
            new Inaddr("nope", -1).failed(),
            Matchers.is(true)
        );
    }

    @Test
    void staysSilentAboutTheLimitedBroadcastAddress() {
        MatcherAssert.assertThat(
            "the limited-broadcast address converts to INADDR_NONE like unconvertible text does, but it is valid, so it must not be reported as a failure",
            new Inaddr("255.255.255.255", -1).failed(),
            Matchers.is(false)
        );
    }

    @Test
    void staysSilentAboutAnAddressItConverted() {
        MatcherAssert.assertThat(
            "a conversion that worked must not be reported as a failure",
            new Inaddr("127.0.0.1", 16_777_343).failed(),
            Matchers.is(false)
        );
    }
}
