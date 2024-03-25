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

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.io.ResourceOf;
import org.cactoos.scalar.LengthOf;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link DcsDepgraph.DcsJson}.
 *
 * @since 0.28.11
 */
final class DcsDepgraphTest {

    @ParameterizedTest
    @CsvSource({
        "eo-math-dependencies-transient-dependency.json, 3",
        "eo-math-dependencies-without-foreign.json, 7"
    })
    void readsAllDependenciesFromJsonFile(
        final String name,
        final long number,
        @TempDir final Path tmp
    ) throws Exception {
        MatcherAssert.assertThat(
            new LengthOf(new DcsDepgraph.DcsJson(this.file(tmp, name))).value(),
            Matchers.equalTo(number)
        );
    }

    private Path file(final Path tmp, final String name) {
        try {
            final Path res = tmp.resolve(name);
            new HmBase(tmp).save(
                new ResourceOf(
                    String.format("org/eolang/maven/dependencies/%s", name)
                ),
                Paths.get(name)
            );
            return res;
        } catch (final IOException ex) {
            throw new IllegalStateException(ex);
        }
    }
}
