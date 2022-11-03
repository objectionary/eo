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

import java.util.Collections;
import org.apache.maven.model.Dependency;
import org.cactoos.scalar.LengthOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link org.eolang.maven.DcsNoOneHasTransitive}.
 *
 * @since 0.28.11
 */
class DcsNoOneHasTransitiveTest {

    @Test
    void checksAllDependenciesWithoutTransitive() throws Exception {
        MatcherAssert.assertThat(
            new LengthOf(
                new DcsNoOneHasTransitive(
                    DcsNoOneHasTransitiveTest.single("eo-collections"),
                    dependency -> DcsNoOneHasTransitiveTest.empty()
                )).value(),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void checksAllDependenciesWithTransitive() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new DcsNoOneHasTransitive(
                DcsNoOneHasTransitiveTest.single("eo-foo"),
                dependency -> DcsNoOneHasTransitiveTest.single("eo-bar")
            )::iterator
        );
    }

    private static Dependencies empty() {
        return Collections::emptyIterator;
    }

    private static Dependencies single(final String artifact) {
        return Collections.singletonList(
            DcsNoOneHasTransitiveTest.dependency(artifact)
        )::iterator;
    }

    private static Dependency dependency(final String artifact) {
        final Dependency dependency = new Dependency();
        dependency.setGroupId("org.eolang");
        dependency.setArtifactId(artifact);
        dependency.setVersion("0.1.0");
        dependency.setScope("compile");
        return dependency;
    }
}
