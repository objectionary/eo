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
package org.eolang.maven;

import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link Unplace}.
 *
 * @since 0.11
 */
final class UnplaceTest {

    @ParameterizedTest
    @CsvSource({
        "/tmp/foo/bar, /tmp/foo/bar/a/b/c.eo, a.b.c",
        "/tmp/foo/bar, /tmp/foo/bar/a/b/.cd.ef.eo, a.b..cd.ef"
    })
    void makesName(
        final String base,
        final String source,
        final String name
    ) {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new Unplace(Paths.get(base)).make(
                Paths.get(source)
            ),
            Matchers.equalTo(name)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "/tmp/foo/bar, /tmp/foo/bar/a/b/c.phi, a.b.c",
        "/tmp/foo/bar, /tmp/foo/bar/a/b/.cd.ef.phi, a.b..cd.ef"
    })
    void makesNameWithCustomExtension(
        final String base,
        final String source,
        final String name
    ) {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new Unplace(Paths.get(base), ".phi").make(
                Paths.get(source)
            ),
            Matchers.equalTo(name)
        );
    }

    @Test
    void interpretsExtensionAsLiteralString() {
        Assertions.assertDoesNotThrow(
            () -> new Unplace(Paths.get("/tmp/foo/bar"), "(")
                .make(Paths.get("/tmp/foo/bar/a/b/c(")),
            "Extension interpreted not as literal string"
        );
    }
}
