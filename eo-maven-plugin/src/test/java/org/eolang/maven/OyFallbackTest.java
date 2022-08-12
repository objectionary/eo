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

import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test for {@link OyFallback}.
 *
 * @since 1.0
 */
final class OyFallbackTest {

    @Test
    void putsObjectToLocalCache(@TempDir final Path path) throws Exception {
        final AtomicInteger counter = new AtomicInteger();
        final String branch = "master";
        final Objectionary objectionary = new OyFallback(
            new OyHome(branch, path),
            new OyCaching(
                branch,
                path,
                name -> {
                    counter.incrementAndGet();
                    return new InputOf("[] > main\n");
                }
            )
        );
        final String object = "org.example.main";
        Assertions.assertNotNull(
            new TextOf(objectionary.get(object)).asString()
        );
        Assertions.assertTrue(
            path.resolve("sources/master/org/example/main.eo").toFile().exists()
        );
        Assertions.assertNotNull(objectionary.get(object));
        MatcherAssert.assertThat(
            counter.get(),
            Matchers.is(1)
        );
    }
}
