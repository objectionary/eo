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

import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Place}.
 *
 * @since 0.11
 */
final class PlaceTest {

    @Test
    void makesPath() {
        MatcherAssert.assertThat(
            new Place("hello.foo.bar")
                .make(Paths.get("/tmp/test"), TranspileMojo.EXT)
                .toString()
                .replace("\\", "/"),
            Matchers.equalTo(String.format("/tmp/test/hello/foo/bar.%s", TranspileMojo.EXT))
        );
    }

    @Test
    void makesSimplePath() {
        MatcherAssert.assertThat(
            new Place("hey")
                .make(Paths.get("/tmp"), "xml")
                .toString()
                .replace("\\", "/"),
            Matchers.equalTo("/tmp/hey.xml")
        );
    }

    @Test
    void makesPathForVersionedObject() {
        final String object = "org.eolang.io.stdout|15c85d7";
        final Path actual = new Place(object)
            .make(Paths.get("/tmp/test"), TranspileMojo.EXT);
        final Path expected = Paths.get("/tmp/test/org/eolang/io/stdout_15c85d7.xmir");
        MatcherAssert.assertThat(
            String.format(
                "Expected path for object '%s' is '%s' but got '%s'",
                object,
                expected,
                actual
            ),
            actual,
            Matchers.equalTo(expected)
        );
    }
}
