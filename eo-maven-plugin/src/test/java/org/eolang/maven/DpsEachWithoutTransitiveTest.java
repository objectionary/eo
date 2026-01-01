/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.stream.Collectors;
import org.cactoos.list.ListOf;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link DpsEachWithoutTransitive}.
 *
 * @since 0.30
 */
final class DpsEachWithoutTransitiveTest {

    @Test
    void failsIfHasTransitiveDependencies() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new DpsEachWithoutTransitive(
                new Dependencies.Fake(),
                dep -> new Dependencies.Fake(100)
            ).iterator().next(),
            "We expect an exception when transitive dependencies are found"
        );
    }

    @Test
    void keepsDependenciesThatHaveTeStDependenciesAsTransitive() {
        final Dependencies original = new Dependencies.Fake();
        MatcherAssert.assertThat(
            "Dependencies should be kept as transitive",
            new DpsEachWithoutTransitive(
                original,
                dep -> new Dependencies.Fake(Dependencies.Fake.randDep("test"))
            ),
            DpsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    @Test
    void keepsDependencyThatHasTheSameDependencyAsTransitive() {
        final Dependencies original = new Dependencies.Fake();
        MatcherAssert.assertThat(
            "Dependencies that have the same dependency should be kept as transitive",
            new DpsEachWithoutTransitive(
                original,
                Dependencies.Fake::new
            ),
            DpsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    @Test
    void keepsDependencyThatHasRuntimeDependencyAsTransitive() {
        final Dependencies original = new Dependencies.Fake();
        MatcherAssert.assertThat(
            "Dependencies with runtime dependencies should be kept as transitive",
            new DpsEachWithoutTransitive(
                original,
                dep -> new Dependencies.Fake(Dependencies.Fake.randDep("test"))
            ),
            DpsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    /**
     * Matcher for checking that iterable has all items from another iterable.
     * @param deps Iterable of dependencies
     * @return Matcher
     */
    private static Matcher<Dependencies> hasAll(final Dependencies deps) {
        return Matchers.allOf(
            new ListOf<>(deps)
                .stream()
                .map(Matchers::hasItem)
                .collect(Collectors.toList())
        );
    }
}
