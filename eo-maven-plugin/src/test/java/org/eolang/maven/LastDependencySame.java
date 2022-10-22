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

import java.util.Deque;
import org.apache.maven.model.Dependency;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;

/**
 * Hamcrest matcher for checking the last dependency.
 *
 * @since 0.28.11
 */
final class LastDependencySame extends TypeSafeMatcher<Dependency> {

    /**
     * All dependencies.
     */
    private final Deque<Dependency> all;

    /**
     * The main constructor.
     *
     * @param all Dependencies that were saved previously.
     */
    LastDependencySame(final Deque<Dependency> all) {
        this.all = all;
    }

    @Override
    public void describeTo(final Description description) {
        description.appendText("The last dependency is different");
    }

    @Override
    public boolean matchesSafely(final Dependency item) {
        return LastDependencySame.dependenciesEquals(item, this.all.getLast());
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
