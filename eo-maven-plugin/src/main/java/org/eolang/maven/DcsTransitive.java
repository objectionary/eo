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

import java.util.Iterator;
import org.apache.maven.model.Dependency;
import org.cactoos.iterator.Filtered;

/**
 * Transitive dependencies.
 *
 * @since 0.28.11
 */
final class DcsTransitive implements Dependencies {

    /**
     * Delegate dependencies.
     */
    private final Dependencies delegate;

    /**
     * The dependency that transitive dependencies we are interested of.
     */
    private final Dependency origin;

    /**
     * The main contructor.
     *
     * @param delegate Delegate
     * @param origin The dependency that transitive dependencies we are interested of
     */
    DcsTransitive(final Dependencies delegate, final Dependency origin) {
        this.delegate = delegate;
        this.origin = origin;
    }

    @Override
    public Iterator<Dependency> iterator() {
        return new Filtered<>(
            this.delegate.iterator(),
            dependency ->
                () -> DcsTransitive.notRuntime(dependency)
                    && DcsTransitive.notTesting(dependency)
                    && this.notSame(dependency)
        );
    }

    /**
     * Check if it's not the same dependency.
     *
     * @param dep Checked dependency.
     * @return True if different with this.origin.
     */
    private boolean notSame(final Dependency dep) {
        return !(
            dep.getGroupId().equals(this.origin.getGroupId())
                && dep.getArtifactId().equals(this.origin.getArtifactId())
            );
    }

    /**
     * Check if it's not the testing dependency.
     *
     * @param dep Checked dependency.
     * @return True if dependency has not test scope.
     */
    private static boolean notTesting(final Dependency dep) {
        return !dep.getScope().contains("test");
    }

    /**
     * Check if it's not the runtime dependency.
     *
     * @param dep Checked dependency.
     * @return True if it isn't a eo-runtime dependency.
     */
    private static boolean notRuntime(final Dependency dep) {
        return !(
            dep.getGroupId().equals("org.eolang")
                && dep.getArtifactId().equals("eo-runtime")
            );
    }
}
