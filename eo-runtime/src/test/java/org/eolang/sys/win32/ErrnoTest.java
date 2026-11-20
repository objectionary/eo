/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.sys.win32;

import java.security.SecureRandom;
import org.eolang.Dataized;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Errno}.
 *
 * @since 0.77.0
 */
final class ErrnoTest {

    @Test
    void leavesMessageEmptyWhenCallWorked() {
        MatcherAssert.assertThat(
            "the message of a call that worked must be an empty string, but it isnt",
            new Dataized(new Errno(new SecureRandom().nextInt(10_000)).get()).asString(),
            Matchers.emptyString()
        );
    }
}
