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
import java.util.LinkedList;
import java.util.List;
import org.apache.maven.model.Dependency;
import org.cactoos.Func;
import org.cactoos.func.UncheckedFunc;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;

/**
 * Check if all dependencies have transitive dependencies.
 *
 * @since 0.28.11
 * @todo #1361:90min The class without tests. We definitely have to write some tests
 *   in order to make the class possible for future refactoring and easy maintainance.
 */
final class DcsNoOneHasTransitive implements Dependencies {

    /**
     * Source of dependencies to check.
     */
    private final Dependencies delegate;

    /**
     * The function that get all transitive dependencies for the particular one.
     */
    private final UncheckedFunc<Dependency, Dependencies> transitive;

    /**
     * The main constructor.
     *
     * @param delegate Source of dependencies to check
     * @param transitive The function that get all transitive dependencies for the particular one.
     */
    DcsNoOneHasTransitive(
        final Dependencies delegate,
        final Func<Dependency, Dependencies> transitive
    ) {
        this.delegate = delegate;
        this.transitive = new UncheckedFunc<>(transitive);
    }

    @Override
    public Iterator<Dependency> iterator() {
        final List<Dependency> res = new LinkedList<>();
        for (final Dependency dep : this.delegate) {
            if (this.hasTransitive(dep)) {
                throw new IllegalStateException(
                    String.format("%s contains transitive dependencies", dep)
                );
            }
            res.add(dep);
        }
        return res.iterator();
    }

    /**
     * Checks if dependency dep has any transitive dependencies.
     *
     * @param dep Dependency to check.
     * @return True if dep has transitive dependencies.
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    private boolean hasTransitive(final Dependency dep) {
        return new Unchecked<>(
            new LengthOf(
                new DcsTransitive(this.transitive.apply(dep), dep)
            )
        ).value() != 0;
    }
}
