/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import org.apache.maven.model.Model;
import org.apache.maven.project.MavenProject;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link RtPom}.
 *
 * @since 1.0
 */
final class RtPomTest {

    @ParameterizedTest
    @ValueSource(strings = {"tests", "sources"})
    void rejectsClassifiedRuntime(final String classifier) {
        MatcherAssert.assertThat(
            "A classified artifact must not count as the main runtime",
            new RtPom(RtPomTest.project(classifier)).isPresent(),
            Matchers.is(false)
        );
    }

    @ParameterizedTest
    @NullAndEmptySource
    void acceptsUnclassifiedRuntime(final String classifier) {
        MatcherAssert.assertThat(
            "The unclassified runtime must remain recognized",
            new RtPom(RtPomTest.project(classifier)).isPresent(),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @CsvSource({"tests,", ",tests"})
    void selectsMainRuntimeInEitherOrder(final String first, final String second) {
        MatcherAssert.assertThat(
            "The main runtime must win regardless of dependency ordering",
            new RtPom(RtPomTest.project(first, second)).value(),
            Matchers.hasToString("org.eolang:eo-runtime:0.75.0")
        );
    }

    private static MavenProject project(final String... classifiers) {
        final Model model = new Model();
        for (final String classifier : classifiers) {
            model.addDependency(
                new Dep().withGroupId("org.eolang")
                    .withArtifactId("eo-runtime")
                    .withVersion("0.75.0")
                    .withClassifier(classifier)
                    .get()
            );
        }
        return new MavenProject(model);
    }
}
