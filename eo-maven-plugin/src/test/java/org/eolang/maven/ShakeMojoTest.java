/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import javax.xml.transform.TransformerFactory;
import net.sf.saxon.TransformerFactoryImpl;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.footprint.Saved;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link ShakeMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(MktmpResolver.class)
final class ShakeMojoTest {

    @Test
    void shakesSimpleObject(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    "# This unit test is supposed to check the functionality of the corresponding object.\n[] > foo\n".getBytes()
                );
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("register", "parse", "shake");
                f.exec("compile");
            }
        );
        MatcherAssert.assertThat(
            "the .xmir file contains lint defects",
            new XMLDocument(temp.resolve("target/eo/2-shake/foo.xmir")),
            XhtmlMatchers.hasXPaths("/program[not(errors)]")
        );
    }

    @Test
    void skipsAlreadyShaken(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Shake());
        final Path path = maven.result().get(
            String.format("target/%s/foo/x/main.%s", ShakeMojo.DIR, AssembleMojo.XMIR)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(ShakeMojo.class);
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            path.toFile().lastModified(),
            Matchers.is(mtime)
        );
    }

    @Test
    void shakesIfExpired(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final Path tgt = maven
            .withHelloWorld()
            .execute(new FakeMaven.Shake())
            .result()
            .get(
                String.format("target/%s/foo/x/main.%s", ShakeMojo.DIR, AssembleMojo.XMIR)
            );
        final long old = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(10L);
        if (!tgt.toFile().setLastModified(old)) {
            Assertions.fail(String.format("The last modified attribute can't be set for %s", tgt));
        }
        maven.execute(ShakeMojo.class);
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
    void getsAlreadyShakenResultsFromCache(@Mktmp final Path temp) throws Exception {
        final TextOf cached = new TextOf(
            new ResourceOf("org/eolang/maven/main.xml")
        );
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new Saved(
            cached,
            cache
                .resolve(ShakeMojo.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir")
        ).value();
        Files.setLastModifiedTime(
            cache.resolve(
                Paths
                    .get(ShakeMojo.CACHE)
                    .resolve(FakeMaven.pluginVersion())
                    .resolve(hash)
                    .resolve("foo/x/main.xmir")
            ),
            FileTime.fromMillis(System.currentTimeMillis() + 50_000)
        );
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache.toFile())
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Shake());
        MatcherAssert.assertThat(
            "OptimizeMojo must load optimized XMIR from cache",
            new XMLDocument(
                new HmBase(temp).load(
                    Paths.get(
                        String.format(
                            "target/%s/foo/x/main.%s",
                            ShakeMojo.DIR,
                            AssembleMojo.XMIR
                        )
                    )
                ).asBytes()
            ),
            Matchers.is(new XMLDocument(cached.asString()))
        );
    }

    @Test
    void savesOptimizedResultsToCache(@Mktmp final Path temp) throws IOException {
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache.toFile())
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Shake());
        MatcherAssert.assertThat(
            "OptimizeMojo must save optimized XMIR to cache",
            cache.resolve(ShakeMojo.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void optimizesSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withHelloWorld()
            .with("trackTransformationSteps", true)
            .execute(new FakeMaven.Shake())
            .result();
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", ShakeMojo.DIR, AssembleMojo.XMIR)
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
    void optimizesConcurrentlyWithLotsOfPrograms(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int total = 20;
        for (int program = 0; program < total; ++program) {
            maven.withHelloWorld();
        }
        final Map<String, Path> res = maven
            .execute(new FakeMaven.Shake())
            .result();
        for (int program = 0; program < total; ++program) {
            MatcherAssert.assertThat(
                CatalogsTest.TO_ADD_MESSAGE,
                res,
                Matchers.hasKey(
                    String.format(
                        "target/%s/foo/x/main%s.%s",
                        ShakeMojo.DIR,
                        FakeMaven.suffix(program),
                        AssembleMojo.XMIR
                    )
                )
            );
        }
    }

    @Test
    void doesNotCrashesOnError(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "[args] > main",
                    "  seq > @",
                    "    TRUE > x",
                    "    FALSE > x"
                ).with("trackTransformationSteps", true)
                .execute(new FakeMaven.Shake())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", ShakeMojo.DIR, AssembleMojo.XMIR)
            )
        );
    }

    @Test
    void choosesTransformerFactoryOnce() {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
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
                CatalogsTest.TO_ADD_MESSAGE,
                clazz,
                Matchers.typeCompatibleWith(TransformerFactoryImpl.class)
            );
        }
    }
}
