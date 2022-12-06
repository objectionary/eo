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
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.footprint.FtCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChRemote;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ParseMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class ParseMojoTest {

    @Test
    void testSimpleParsing(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            maven.withHelloWorld()
                .execute(ParseMojo.class)
                .result(),
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", ParseMojo.DIR, TranspileMojo.EXT)
            )
        );
        MatcherAssert.assertThat(
            maven.foreign().getById("foo.x.main").exists("xmir"),
            Matchers.is(true)
        );
    }

    @Test
    void failsOnTimeout(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .with("timeout", 0)
                .execute(ParseMojo.class)
        );
    }

    @Test
    void testSimpleParsingCached(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final Path cache = temp.resolve("cache");
        final String expected = new UncheckedText(
            new TextOf(new ResourceOf("org/eolang/maven/main.xmir"))
        ).asString();
        final String hash = new ChNarrow(new ChRemote("0.25.0")).value();
        new FtCached(
            hash,
            maven.targetPath(),
            cache.resolve(ParseMojo.PARSED)
        ).save("foo.x.main", "xmir", () -> expected);
        MatcherAssert.assertThat(
            new TextOf(
                maven.withProgram("invalid content")
                    .withTojoAttribute(AssembleMojo.ATTR_HASH, hash)
                    .with("cache", cache)
                    .execute(ParseMojo.class)
                    .result()
                    .get(String.format("target/%s/foo/x/main.%s", ParseMojo.DIR, TranspileMojo.EXT))
            ).toString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void testCrashOnInvalidSyntax(@TempDir final Path temp) {
        MatcherAssert.assertThat(
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .withProgram("something < is wrong here")
                    .execute(ParseMojo.class)
            ).getCause().getCause().getMessage(),
            Matchers.containsString("Failed to parse")
        );
    }

    @Test
    void testDoNotCrashesWithFailOnError(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withProgram("something < is wrong here")
                .with("failOnError", false)
                .execute(ParseMojo.class)
                .result(),
            Matchers.not(
                Matchers.hasKey(
                    String.format("target/%s/foo/x/main.%s", ParseMojo.DIR, TranspileMojo.EXT)
                )
            )
        );
    }

    /**
     * The test with high number of eo programs reveals concurrency problems of the ParseMojo.
     * Since other tests works only with single program - it's hard to find concurrency mistakes.
     * @param temp Test directory.
     * @throws IOException If problem with filesystem happened.
     */
    @Test
    void parsesConcurrentlyWithLotsOfPrograms(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int total = 50;
        for (int program = 0; program < total; ++program) {
            maven.withHelloWorld();
        }
        final Map<String, Path> res = maven.execute(ParseMojo.class).result();
        for (int program = 0; program < total; ++program) {
            MatcherAssert.assertThat(
                res,
                Matchers.hasKey(
                    String.format(
                        "target/%s/foo/x/main%s.%s",
                        ParseMojo.DIR,
                        FakeMaven.suffix(program),
                        TranspileMojo.EXT
                    ))
            );
        }
    }
}
