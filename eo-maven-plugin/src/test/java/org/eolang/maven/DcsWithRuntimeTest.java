/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.WeAreOnline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link DcsWithRuntime}.
 *
 * @since 0.28.11
 */
final class DcsWithRuntimeTest {

    @Test
    @ExtendWith(WeAreOnline.class)
    void addsHardcodedVersionOfRuntimeDependency() {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new DcsWithRuntime(
                new DcsFake(5),
                DcsFake.runtimeDep()
            ),
            Matchers.iterableWithSize(6)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void addsRemoteVersionOfRuntimeDependency() {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new DcsWithRuntime(
                new DcsFake(2)
            ),
            Matchers.iterableWithSize(3)
        );
    }
}
