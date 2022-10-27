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

import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import org.apache.maven.model.Dependency;
import org.cactoos.Func;
import org.cactoos.Scalar;
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
    public Collection<Dependency> all() {
        return StreamSupport.stream(
            ((Iterable<Dependency>) () -> new Filtered<>(
                new Filtered<>(
                    new Filtered<>(
                        this.delegate.all().iterator(),
                        new NotSame(this.origin)
                    ), new NotRuntime()
                ), new NotTesting()
            )
            ).spliterator(),
                false
            )
            .collect(Collectors.toList());
    }

    /**
     * Filters runtime dependency eo-runtime.
     *
     * @since 0.28.11
     */
    private static final class NotRuntime implements Func<Dependency, Scalar<Boolean>> {

        @Override
        public Scalar<Boolean> apply(final Dependency dependency) throws Exception {
            return () -> !(
                dependency.getGroupId().equals("org.eolang")
                    && dependency.getArtifactId().equals("eo-runtime")
            );
        }
    }

    /**
     * Filters all dependencies with the same group and artifact id.
     *
     * @since 0.28.11
     */
    private static final class NotSame implements Func<Dependency, Scalar<Boolean>> {

        /**
         * Dependency to check.
         */
        private final Dependency current;

        /**
         * The main constructor.
         *
         * @param current Dependency to check
         */
        NotSame(final Dependency current) {
            this.current = current;
        }

        @Override
        public Scalar<Boolean> apply(final Dependency dependency) throws Exception {
            return () -> !(
                dependency.getGroupId().equals(this.current.getGroupId())
                    && dependency.getArtifactId().equals(this.current.getArtifactId())
            );
        }
    }

    /**
     * Filters all test dependencies.
     *
     * @since 0.28.11
     */
    @SuppressWarnings("PMD.JUnit4TestShouldUseTestAnnotation")
    private static final class NotTesting implements Func<Dependency, Scalar<Boolean>> {

        @Override
        public Scalar<Boolean> apply(final Dependency dependency) throws Exception {
            return () -> !dependency.getScope().contains("test");
        }
    }
}
