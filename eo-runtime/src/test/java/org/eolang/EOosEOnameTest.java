/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link EOos$EOname}.
 *
 * @since 0.40
 */
final class EOosEOnameTest {

    @Test
    void readsSystemFamily() {
        MatcherAssert.assertThat(
            "Dataization of uname object should not return null",
            new Dataized(
                new EOos$EOname()
            ).take(String.class),
            Matchers.is(Matchers.notNullValue())
        );
    }

    @Test
    void readsSystemFamilyCorrectly() {
        MatcherAssert.assertThat(
            "Object uname returns incorrect system name",
            new Dataized(
                new EOos$EOname()
            ).take(String.class),
            Matchers.equalTo(System.getProperty("os.name"))
        );
    }
}
