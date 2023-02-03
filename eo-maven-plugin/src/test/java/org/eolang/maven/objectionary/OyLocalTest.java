/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven.objectionary;

import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test for {@link OyHome}.
 *
 * @since 1.0
 */
final class OyLocalTest {

    @Test
    void resolvesObjectInLocalStorage(@TempDir final Path path) throws Exception {
        final String content = "[] > main\n";
        new Home(path).save(
            content,
            Paths.get("pulled/master/org/example/main.eo")
        );
        MatcherAssert.assertThat(
            new TextOf(
                new OyHome("master", path)
                    .get("org.example.main")
            ).asString(),
            Matchers.is(content)
        );
    }

    @Test
    void checksPresenceOfObjectInLocalStorage(@TempDir final Path path) throws Exception {
        final String content = "[] > main\n";
        new Home(path).save(
            content,
            Paths.get("pulled/master/org/example/main.eo")
        );
        MatcherAssert.assertThat(
            new TextOf(
                new OyHome("master", path)
                    .get("org.example.main")
            ).asString(),
            Matchers.is(content)
        );
        MatcherAssert.assertThat(
            new OyHome("master", path)
                .contains("org.example.main"),
            Matchers.is(true)
        );
    }

}
