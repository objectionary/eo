/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.dependencies;

import java.util.Collections;
import java.util.stream.Collectors;
import org.apache.maven.model.Dependency;
import org.cactoos.list.ListOf;
import org.hamcrest.Matcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link DcsEachWithoutTransitive}.
 *
 * @since 0.30
 */
class DcsEachWithoutTransitiveTest {

    @Test
    void failsIfHasTransitiveDependencies() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new DcsEachWithoutTransitive(
                new DcsFake(),
                dep -> new DcsFake(100)
            ).iterator().next()
        );
    }

    @Test
    void keepsDependenciesThatHaveTeStDependenciesAsTransitive() {
        final DcsFake original = new DcsFake();
        MatcherAssert.assertThat(
            new DcsEachWithoutTransitive(
                original,
                dep -> Collections.singleton(DcsFake.randDep("test"))
            ),
            DcsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    @Test
    void keepsDependencyThatHasTheSameDependencyAsTransitive() {
        final DcsFake original = new DcsFake();
        MatcherAssert.assertThat(
            new DcsEachWithoutTransitive(
                original,
                DcsFake::new
            ),
            DcsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    @Test
    void keepsDependencyThatHasRuntimeDependencyAsTransitive() {
        final DcsFake original = new DcsFake();
        MatcherAssert.assertThat(
            new DcsEachWithoutTransitive(
                original,
                dep -> Collections.singleton(DcsFake.runtimeDep())
            ),
            DcsEachWithoutTransitiveTest.hasAll(original)
        );
    }

    /**
     * Matcher for checking that iterable has all items from another iterable.
     * @param deps Iterable of dependencies
     * @return Matcher
     */
    private static Matcher<? super Iterable<Dependency>> hasAll(
        final Iterable<? extends Dependency> deps
    ) {
        return Matchers.allOf(
            new ListOf<>(deps)
                .stream()
                .map(Matchers::hasItem)
                .collect(Collectors.toList())
        );
    }
}
