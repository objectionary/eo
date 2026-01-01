/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link DpsWithRuntime}.
 *
 * @since 0.28.11
 */
final class DpsWithRuntimeTest {

    @Test
    @ExtendWith(WeAreOnline.class)
    void addsHardcodedVersionOfRuntimeDependency() {
        final int expected = 6;
        MatcherAssert.assertThat(
            String.format(
                "Expected %d dependencies when adding runtime dependency",
                expected
            ),
            new DpsWithRuntime(
                new Dependencies.Fake(5),
                Dependencies.Fake.runtimeDep()
            ),
            Matchers.iterableWithSize(expected)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void addsRemoteVersionOfRuntimeDependency() {
        final int expected = 3;
        MatcherAssert.assertThat(
            String.format(
                "Expected %d dependencies when adding the remote version of runtime dependency",
                expected
            ),
            new DpsWithRuntime(
                new Dependencies.Fake(2)
            ),
            Matchers.iterableWithSize(expected)
        );
    }
}
