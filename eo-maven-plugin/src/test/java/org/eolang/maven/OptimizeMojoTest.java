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
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import javax.xml.transform.TransformerFactory;
import net.sf.saxon.TransformerFactoryImpl;
import org.cactoos.Text;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.maven.util.HmBase;
import org.eolang.parser.CheckPack;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link OptimizeMojo}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
final class OptimizeMojoTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/packs/", glob = "**.yaml")
    void checksPacks(final String pack) throws IOException {
        final CheckPack check = new CheckPack(pack);
        if (check.skip()) {
            Assumptions.abort(
                String.format("%s is not ready", pack)
            );
        }
        MatcherAssert.assertThat(
            check.failures(),
            Matchers.empty()
        );
    }

    @Test
    void skipsAlreadyOptimized(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Optimize());
        final Path path = maven.result().get(
            String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(OptimizeMojo.class);
        MatcherAssert.assertThat(
            path.toFile().lastModified(),
            Matchers.is(mtime)
        );
    }

    @Test
    void optimizesIfExpired(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final Path tgt = maven
            .withHelloWorld()
            .execute(new FakeMaven.Optimize())
            .result()
            .get(
                String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
            );
        final long old = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(10L);
        if (!tgt.toFile().setLastModified(old)) {
            Assertions.fail(String.format("The last modified attribute can't be set for %s", tgt));
        }
        maven.execute(OptimizeMojo.class);
        MatcherAssert.assertThat(
            "We expect that already optimized xmir will be replaced by a new optimized xmir, because the first xmir is outdated and should be updated",
            tgt.toFile().lastModified(),
            Matchers.greaterThan(old)
        );
    }

    /**
     * Test case for #1223.
     *
     * @param temp Temporary test directory.
     * @throws Exception if unexpected error happened.
     */
    @Test
    void getsAlreadyOptimizedResultsFromCache(@TempDir final Path temp) throws Exception {
        final Text cached = new TextOf(
            new EoSyntax(
                "test-it-4",
                new InputOf(
                    String.join(
                        "\n",
                        "+alias stdout org.eolang.io.stdout",
                        "+package f\n",
                        "[x] > main",
                        "  (stdout \"Hello!\" x).print > @"
                    )
                )
            ).parsed().toString()
        );
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new HmBase(cache).save(
            cached,
            Paths.get(OptimizeMojo.OPTIMIZED)
                .resolve(hash)
                .resolve("foo/x/main.xmir")
        );
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache)
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Optimize());
        MatcherAssert.assertThat(
            new XMLDocument(
                new HmBase(temp).load(
                    Paths.get(
                        String.format(
                            "target/%s/foo/x/main.%s",
                            OptimizeMojo.DIR,
                            TranspileMojo.EXT
                        )
                    )
                ).asBytes()
            ),
            Matchers.is(new XMLDocument(cached.asString()))
        );
    }

    @Test
    void savesOptimizedResultsToCache(@TempDir final Path temp) throws IOException {
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache)
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Optimize());
        MatcherAssert.assertThat(
            cache.resolve(OptimizeMojo.OPTIMIZED)
                .resolve(hash)
                .resolve("foo/x/main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void optimizesSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withHelloWorld()
            .with("trackOptimizationSteps", true)
            .execute(new FakeMaven.Optimize())
            .result();
        MatcherAssert.assertThat(
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main/01-not-empty-atoms.xml", OptimizeMojo.STEPS)
            )
        );
        MatcherAssert.assertThat(
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
            )
        );
    }

    /**
     * The test with high number of eo programs reveals concurrency problems of the OptimizeMojo.
     * Since other tests works only with single program - it's hard to find concurrency mistakes.
     * @param temp Test directory.
     * @throws java.io.IOException If problem with filesystem happened.
     */
    @Test
    void optimizesConcurrentlyWithLotsOfPrograms(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int total = 20;
        for (int program = 0; program < total; ++program) {
            maven.withHelloWorld();
        }
        final Map<String, Path> res = maven
            .execute(new FakeMaven.Optimize())
            .result();
        for (int program = 0; program < total; ++program) {
            MatcherAssert.assertThat(
                res,
                Matchers.hasKey(
                    String.format(
                        "target/%s/foo/x/main%s.%s",
                        OptimizeMojo.DIR,
                        FakeMaven.suffix(program),
                        TranspileMojo.EXT
                    )
                )
            );
        }
    }

    @Test
    void doesNotCrashesOnError(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "[args] > main",
                    "  seq > @",
                    "    TRUE > x",
                    "    FALSE > x"
                ).with("trackOptimizationSteps", true)
                .execute(new FakeMaven.Optimize())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
            )
        );
    }

    @Test
    void choosesTransformerFactoryOnce() {
        MatcherAssert.assertThat(
            TransformerFactory.newInstance().getClass(),
            Matchers.typeCompatibleWith(TransformerFactoryImpl.class)
        );
    }

    @Test
    void choosesTransformerFactoryInConcurrentEnvironment() {
        for (final Class<? extends TransformerFactory> clazz : IntStream.range(0, 100).parallel()
            .mapToObj(i -> TransformerFactory.newInstance().getClass())
            .collect(Collectors.toList())) {
            MatcherAssert.assertThat(
                clazz,
                Matchers.typeCompatibleWith(TransformerFactoryImpl.class)
            );
        }
    }
}
