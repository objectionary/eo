/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import java.nio.file.Path;
import java.util.Deque;
import java.util.LinkedList;
import java.util.function.BiConsumer;
import org.apache.maven.model.Dependency;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;

/**
 * The class for emulating of Maven Central repository.
 *
 * @since 0.28.11
 */
final class MockMavenCentral implements BiConsumer<Dependency, Path> {

    /**
     * All saved dependencies.
     */
    private final Deque<Dependency> dependencies;

    /**
     * All paths where dependencies were saved.
     */
    private final Deque<Path> paths;

    /**
     * Default constructor with predefined containers.
     */
    MockMavenCentral() {
        this(new LinkedList<>(), new LinkedList<>());
    }

    /**
     * The main constructor.
     *
     * @param dependencies Dependencies container
     * @param paths Paths container
     */
    private MockMavenCentral(
        final Deque<Dependency> dependencies,
        final Deque<Path> paths
    ) {
        this.dependencies = dependencies;
        this.paths = paths;
    }

    @Override
    public void accept(final Dependency dependency, final Path path) {
        this.dependencies.addLast(dependency);
        this.paths.addLast(path);
    }

    /**
     * The method check the last saved dependency.
     *
     * @param dependency Expected dependency that was downloaded the last
     * @param path Expected path where we saved the last dependency
     */
    void assertLastDownloadedDependencyAndPathEquals(final Dependency dependency, final Path path) {
        MatcherAssert.assertThat(this.dependencies, Matchers.not(Matchers.empty()));
        MatcherAssert.assertThat(this.paths, Matchers.not(Matchers.empty()));
        final Dependency dep = this.dependencies.getLast();
        final Path actual = this.paths.getLast();
        MatcherAssert.assertThat(true, Matchers.is(dependenciesEquals(dep, dependency)));
        MatcherAssert.assertThat(actual, Matchers.equalTo(path));
    }

    /**
     * The method checks if dependencies container is empty.
     *
     * @return True if at least one dependency was accepted
     */
    boolean isNotEmpty() {
        return !this.dependencies.isEmpty();
    }

    /**
     * Compare dependency fields.
     *
     * @param expected Expected dependency
     * @param actual Actual dependency
     * @return True if the main properties are equal
     */
    private static boolean dependenciesEquals(
        final Dependency expected,
        final Dependency actual
    ) {
        return theSameMainInformation(expected, actual)
            && theSameTypeAndClassifier(expected, actual);
    }

    /**
     * Compare dependency fields.
     *
     * @param expected Expected dependency
     * @param actual Actual dependency
     * @return True if group, artifact and version are the same
     */
    private static boolean theSameMainInformation(
        final Dependency expected,
        final Dependency actual
    ) {
        return actual.getGroupId().equals(expected.getGroupId())
            && actual.getArtifactId().equals(expected.getArtifactId())
            && actual.getVersion().equals(expected.getVersion());
    }

    /**
     * Compare dependency fields.
     *
     * @param expected Expected dependency
     * @param actual Actual dependency
     * @return True of type and classifier the same
     */
    private static boolean theSameTypeAndClassifier(
        final Dependency expected,
        final Dependency actual
    ) {
        return actual.getType().equals(expected.getType())
            && actual.getClassifier().equals(expected.getClassifier());
    }
}
