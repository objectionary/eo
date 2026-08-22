/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Dep}.
 * @since 0.56.9
 */
final class DepTest {

    @Test
    void equalsToAnotherDepWithSameCoordinate() {
        MatcherAssert.assertThat(
            "Two deps with the same coordinate must be equal",
            DepTest.runtime(),
            Matchers.equalTo(DepTest.runtime())
        );
    }

    @Test
    void doesNotEqualToDepWithAnotherVersion() {
        MatcherAssert.assertThat(
            "Deps with different versions must not be equal",
            DepTest.runtime(),
            Matchers.not(
                Matchers.equalTo(
                    new Dep()
                        .withGroupId("org.eolang")
                        .withArtifactId("eo-runtime")
                        .withVersion("0.0.1")
                )
            )
        );
    }

    @Test
    void doesNotEqualToItsOwnCoordinateString() {
        MatcherAssert.assertThat(
            "Dep must not be equal to a plain string, since that breaks symmetry",
            DepTest.runtime(),
            Matchers.not(Matchers.<Object>equalTo("org.eolang:eo-runtime:0.0.0"))
        );
    }

    @Test
    void doesNotEqualToWrappedDependency() {
        MatcherAssert.assertThat(
            "Dep must not be equal to the Maven dependency it wraps",
            DepTest.runtime(),
            Matchers.not(Matchers.<Object>equalTo(DepTest.runtime().get()))
        );
    }

    @Test
    void makesSameHashCodeForSameCoordinate() {
        MatcherAssert.assertThat(
            "Equal deps must have equal hash codes",
            DepTest.runtime().hashCode(),
            Matchers.equalTo(DepTest.runtime().hashCode())
        );
    }

    private static Dep runtime() {
        return new Dep()
            .withGroupId("org.eolang")
            .withArtifactId("eo-runtime")
            .withVersion("0.0.0");
    }
}
