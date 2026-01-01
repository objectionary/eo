/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Tests for {@link DpsOfflineRuntime}.
 *
 * @since 0.56.5
 */
final class DpsOfflineRuntimeTest {

    @Test
    void addsCurrentEoDependency() {
        MatcherAssert.assertThat(
            "Offline EO runtime dependency does not match with expected",
            new DpsOfflineRuntime(new ListOf<>()),
            Matchers.hasItem(
                Matchers.hasToString(
                    Matchers.containsString("org.eolang:eo-runtime")
                )
            )
        );
    }

    @Test
    void doesNotDuplicatesRuntime() {
        MatcherAssert.assertThat(
            "Size of dependencies does not match with expected",
            new DpsOfflineRuntime(
                new ListOf<>(
                    new Dep().withGroupId("org.eolang")
                        .withArtifactId("eo-runtime")
                        .withVersion("0.56.2")
                )
            ),
            Matchers.iterableWithSize(1)
        );
    }

    @Test
    void usesAlreadyDefinedVersion() {
        MatcherAssert.assertThat(
            "Offline EO runtime dependency should use already defined version",
            new DpsOfflineRuntime(
                new ListOf<>(
                    new Dep().withGroupId("org.eolang")
                        .withArtifactId("eo-runtime")
                        .withVersion("0.0.1")
                )
            ),
            Matchers.hasItem(
                Matchers.hasToString(
                    Matchers.containsString("org.eolang:eo-runtime:0.0.1")
                )
            )
        );
    }
}
