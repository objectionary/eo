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
package org.eolang.maven.optimization;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.UncheckedBytes;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link org.eolang.maven.optimization.OptCached}.
 * @since 0.28.12
 */
class OptCachedTest {
    @Test
    void optimizesIfXmlAlreadyInCache(final @TempDir Path tmp) throws IOException {
        final XML program = OptCachedTest.program();
        OptCachedTest.save(tmp, program);
        MatcherAssert.assertThat(
            new OptCached(path -> program, tmp).apply(program),
            Matchers.equalTo(program)
        );
    }

    @Test
    void optimizesIfXmlIsAbsentInCache(final @TempDir Path tmp) {
        final XML program = OptCachedTest.program();
        final Path cache = tmp.resolve("cache");
        final XML res = new OptCached(path -> program, cache)
            .apply(program);
        MatcherAssert.assertThat(
            res,
            Matchers.equalTo(program)
        );
        MatcherAssert.assertThat(
            cache.resolve("main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            res, Matchers.equalTo(program)
        );
    }

    /**
     * Save XML program with hardcoded name to a temp directory.
     * @param tmp Temporary test directory.
     * @param xml XML program.
     * @return Path to saved program.
     */
    private static Path save(final Path tmp, final XML xml) throws IOException {
        final Path path = Paths.get("main.xmir");
        final Path res = tmp.resolve(path);
        new Home(tmp).save(xml.toString().getBytes(StandardCharsets.UTF_8), path);
        return res;
    }

    /**
     * Get parsed program from resources.
     * @return XML representation of program.
     */
    private static XML program() {
        return new XMLDocument(
            new UncheckedBytes(
                new BytesOf(
                    new ResourceOf("org/eolang/maven/optimize/main.xml")
                )
            ).asBytes()
        );
    }
}
