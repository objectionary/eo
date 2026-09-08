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
 * Test case for {@link DpsWithoutRuntime}.
 * @since 0.29
 */
final class DpsWithoutRuntimeTest {

    @Test
    void removesTheRuntimeOfEo() {
        MatcherAssert.assertThat(
            "the EO runtime must be taken out of the list, but it wasnt",
            new DpsWithoutRuntime(
                () -> new ListOf<>(
                    new Dep()
                        .withGroupId("org.eolang")
                        .withArtifactId("eo-runtime")
                        .withVersion("0.30.0")
                ).iterator()
            ),
            Matchers.emptyIterable()
        );
    }

    @Test
    void keepsTheRuntimeOfSomebodyElse() {
        MatcherAssert.assertThat(
            "a dependency of another group named eo-runtime must stay, but it didnt",
            new DpsWithoutRuntime(
                () -> new ListOf<>(
                    new Dep()
                        .withGroupId("com.example")
                        .withArtifactId("eo-runtime")
                        .withVersion("1.0.0")
                ).iterator()
            ),
            Matchers.iterableWithSize(1)
        );
    }
}
