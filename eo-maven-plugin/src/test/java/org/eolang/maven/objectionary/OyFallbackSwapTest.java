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
package org.eolang.maven.objectionary;

import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Tests for {@link OyFallbackSwap}.
 * @since 1.0
 */
class OyFallbackSwapTest {
    @Test
    void getsWithFallbackNoSwapOy() throws Exception {
        MatcherAssert.assertThat(
            new TextOf(
                new OyFallbackSwap(
                    new Objectionary.Fake(s -> new InputOf("[] > local\n")),
                    new Objectionary.Fake(s -> new InputOf("[] > remote\n")),
                    false
                ).get("")
            ).asString(),
            Matchers.containsString("local")
        );
    }

    @Test
    void getsWithFallbackSwapOyFail() throws Exception {
        MatcherAssert.assertThat(
            new TextOf(
                new OyFallbackSwap(
                    new Objectionary.Fake(s -> new InputOf("[] > local\n")),
                    new Objectionary.Fake(
                        s -> {
                            throw new IOException("Can't get object");
                        }
                    ),
                    false
                ).get("")
            ).asString(),
            Matchers.containsString("local")
        );
    }

    @Test
    void getsWithFallbackSwapOy() throws Exception {
        MatcherAssert.assertThat(
            new TextOf(
                new OyFallbackSwap(
                    new Objectionary.Fake(s -> new InputOf("[] > local\n")),
                    new Objectionary.Fake(s -> new InputOf("[] > remote\n")),
                    true
                ).get("")
            ).asString(),
            Matchers.containsString("remote")
        );
    }

    @Test
    void checksPresenceOfObject(@TempDir final Path path) throws Exception {
        final Objectionary home = new OyHome(
            "master",
            path
        );
        final Objectionary cache = new OyCaching(
            "master",
            path,
            new Objectionary.Fake(
                s -> new InputOf("[] > main\n"),
                s -> false
            )
        );
        MatcherAssert.assertThat(
            new OyFallbackSwap(
                home,
                cache,
                home.contains("")
            ).contains(""),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            new TextOf(
                new OyFallbackSwap(
                    home,
                    cache,
                    true
                ).get("")
            ).asString(),
            Matchers.is(Matchers.notNullValue())
        );
        MatcherAssert.assertThat(
            new OyFallbackSwap(
                home,
                cache,
                home.contains("")
            ).contains(""),
            Matchers.is(true)
        );
    }
}
