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

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import org.apache.maven.model.Dependency;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link TransitiveDependencies}.
 *
 * @since 0.28.11
 */
class TransitiveDependenciesTest {

    @ParameterizedTest
    @CsvSource({
        "eo-math-dependencies-transient-dependency.json, false",
        "eo-math-dependencies-without-foreign.json, true"

    })
    void selectsOnlyNonTestNonRuntimeNonSameDependencies(
        final String name,
        final boolean empty,
        @TempDir final Path tmp
    ) {
        final Path file = this.dependenciesJson(tmp, name);
        final List<Dependency> all = new TransitiveDependencies(
            file,
            this.sameDependency()
        ).toList();
        MatcherAssert.assertThat(all.isEmpty(), Matchers.is(empty));
    }

    private Dependency sameDependency() {
        final Dependency dependency = new Dependency();
        dependency.setVersion("0.2.3");
        dependency.setArtifactId("eo-math");
        dependency.setGroupId("org.eolang");
        return dependency;
    }

    private Path dependenciesJson(final Path tmp, final String name) {
        try {
            final Path res = tmp.resolve(name);
            new Home().save(
                new ResourceOf(
                    String.format("org/eolang/maven/dependencies/%s", name)
                ),
                res
            );
            return res;
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }
}
