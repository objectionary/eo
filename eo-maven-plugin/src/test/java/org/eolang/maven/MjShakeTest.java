/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.TrJoined;
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
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.parser.StFlatBytes;
import org.eolang.parser.TrFull;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtStrict;
import org.eolang.xax.XtYaml;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link MjShake}.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(MktmpResolver.class)
final class MjShakeTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/shake-packs/", glob = "**.yaml")
    void checksShakePacks(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            new XtSticky(
                new XtStrict(
                    new XtYaml(
                        yaml,
                        eo -> new EoSyntax("scenario", String.format("%s\n", eo)).parsed(),
                        new TrJoined<>(
                            new TrDefault<>(new StFlatBytes()),
                            new TrFull()
                        )
                    )
                )
            ),
            new XtoryMatcher()
        );
    }

    @Test
    void skipsAlreadyShaken(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Shake());
        final Path path = maven.result().get(
            String.format("target/%s/foo/x/main.%s", MjShake.DIR, MjAssemble.XMIR)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(MjShake.class);
        MatcherAssert.assertThat(
            "ShakeMojo must skip when already shaken, but it doesn't",
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
                String.format("target/%s/foo/x/main.%s", MjShake.DIR, MjAssemble.XMIR)
            );
        final long old = System.currentTimeMillis() - TimeUnit.DAYS.toMillis(10L);
        if (!tgt.toFile().setLastModified(old)) {
            Assertions.fail(String.format("The last modified attribute can't be set for %s", tgt));
        }
        maven.execute(MjShake.class);
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
                .resolve(MjShake.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir")
        ).value();
        Files.setLastModifiedTime(
            cache.resolve(
                Paths
                    .get(MjShake.CACHE)
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
                Files.readAllBytes(
                    temp.resolve(
                        String.format(
                            "target/%s/foo/x/main.%s",
                            MjShake.DIR,
                            MjAssemble.XMIR
                        )
                    )
                )
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
            cache.resolve(MjShake.CACHE)
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
            "ShakeMojo must successfully optimize, but it doesn't",
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", MjShake.DIR, MjAssemble.XMIR)
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
                "ShakeMojo must successfully optimize concurrently, but it doesn't",
                res,
                Matchers.hasKey(
                    String.format(
                        "target/%s/foo/x/main%s.%s",
                        MjShake.DIR,
                        FakeMaven.suffix(program),
                        MjAssemble.XMIR
                    )
                )
            );
        }
    }

    @Test
    void doesNotCrashesOnError(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "ShakeMojo must not crash when an incorrect program",
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
                String.format("target/%s/foo/x/main.%s", MjShake.DIR, MjAssemble.XMIR)
            )
        );
    }

    @Test
    void choosesTransformerFactoryOnce() {
        MatcherAssert.assertThat(
            "TransformerFactory instance should be of type TransformerFactoryImpl",
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
                String.join(
                    " ",
                    "TransformerFactory instance should be of type TransformerFactoryImpl",
                    "in concurrent environment"
                ),
                clazz,
                Matchers.typeCompatibleWith(TransformerFactoryImpl.class)
            );
        }
    }
}
