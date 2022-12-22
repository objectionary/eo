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
package org.eolang.maven.objectionary;

import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.OyFake;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test for {@link OyFallback}.
 *
 * @since 1.0
 * @todo #1567:30min The "putsObjectToLocalCache" is very complicated, since
 *  it checks a lot of things like methods "get" and "contains" in Objectionary,
 *  work of "OyFallback", and behaviour of "OyCaching". Need to split this test
 *  into several tests to check every thing separately.
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
                new OyFake(
                    s -> {
                        counter.incrementAndGet();
                        return new InputOf("[] > main\n");
                    },
                    s -> {
                        counter.incrementAndGet();
                        return false;
                    }
                )
            )
        );
        final String object = "org.example.main";
        MatcherAssert.assertThat(
            objectionary.contains(object),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            new TextOf(objectionary.get(object)).asString(),
            Matchers.is(Matchers.notNullValue())
        );
        MatcherAssert.assertThat(
            path.resolve("pulled/master/org/example/main.eo").toFile().exists(),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            objectionary.contains(object),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            objectionary.get(object),
            Matchers.is(Matchers.notNullValue())
        );
        MatcherAssert.assertThat(
            counter.get(),
            Matchers.is(2)
        );
    }
}
