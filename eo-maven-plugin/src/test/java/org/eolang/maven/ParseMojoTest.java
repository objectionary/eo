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

import com.yegor256.WeAreOnline;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.footprint.CacheVersion;
import org.eolang.maven.footprint.FtCached;
import org.eolang.maven.footprint.FtDefault;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.CommitHash;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ParseMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class ParseMojoTest {

    @Test
    void parsesSuccessfully(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            maven.withHelloWorld()
                .execute(new FakeMaven.Parse())
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
                .execute(InfiniteMojo.class)
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void parsesWithCache(@TempDir final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram("invalid content")
            .with("cache", cache);
        final String expected = new UncheckedText(
            new TextOf(new ResourceOf("org/eolang/maven/main.xmir"))
        ).asString();
        final CommitHash hash = new ChCached(new ChNarrow(new ChRemote("0.25.0")));
        new FtCached(
            new CacheVersion(FakeMaven.pluginVersion(), hash.value()),
            cache.resolve(ParseMojo.PARSED),
            new FtDefault(maven.targetPath())
        ).save("foo.x.main", "xmir", () -> expected);
        final String actual = String.format(
            "target/%s/foo/x/main.%s",
            ParseMojo.DIR,
            TranspileMojo.EXT
        );
        MatcherAssert.assertThat(
            String.format("We expect that that %s is taken from the cache, but it didn't", actual),
            new TextOf(
                maven.allTojosWithHash(hash)
                    .execute(new FakeMaven.Parse())
                    .result()
                    .get(actual)
            ).toString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void doesNotCrashesOnError(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withProgram("something < is wrong here")
                .execute(new FakeMaven.Parse())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", ParseMojo.DIR, TranspileMojo.EXT)
            )
        );
    }

    @Test
    void doesNotParseIfAlreadyParsed(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> result = maven
            .withHelloWorld()
            .execute(new FakeMaven.Parse())
            .result();
        final File parsed = result.get(
            String.format("target/%s/foo/x/main.%s", ParseMojo.DIR, TranspileMojo.EXT)
        ).toFile();
        final long before = parsed.lastModified();
        maven.execute(ParseMojo.class);
        final long after = parsed.lastModified();
        MatcherAssert.assertThat(
            "File was modified",
            before,
            Matchers.equalTo(after)
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
        final Map<String, Path> res = maven.execute(new FakeMaven.Parse()).result();
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

    /**
     * The mojo that does nothing, but executes infinitely.
     * @since 0.29
     */
    @Mojo(name = "infinite", defaultPhase = LifecyclePhase.VALIDATE)
    private static final class InfiniteMojo extends SafeMojo {
        @Override
        public void exec() {
            try {
                Thread.sleep(Long.MAX_VALUE);
            } catch (final InterruptedException ex) {
                Thread.currentThread().interrupt();
                throw new IllegalStateException(ex);
            }
        }
    }
}
