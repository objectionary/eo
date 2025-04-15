/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.Map;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjParse}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
@ExtendWith(MktmpResolver.class)
final class MjParseTest {

    @Test
    void parsesSuccessfully(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final String parsed = String.format(
            "target/%s/foo/x/main.%s",
            MjParse.DIR,
            MjAssemble.XMIR
        );
        MatcherAssert.assertThat(
            String.format("ParseMojo should have parsed stdout object %s, but didn't", parsed),
            maven.withHelloWorld()
                .execute(new FakeMaven.Parse())
                .result(),
            Matchers.hasKey(parsed)
        );
        MatcherAssert.assertThat(
            "The resource must exist, but it doesn't",
            maven.foreign().getById("foo.x.main").exists("xmir"),
            Matchers.is(true)
        );
    }

    @Test
    void failsOnTimeout(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .with("timeout", 0)
                .execute(Infinite.class),
            "Expected IllegalStateException on timeout"
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void parsesWithCache(@Mktmp final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram("invalid content")
            .with("cache", cache.toFile());
        final String expected = new UncheckedText(
            new TextOf(new ResourceOf("org/eolang/maven/main.xmir"))
        ).asString();
        final CommitHash hash = new ChCached(new ChNarrow(new ChRemote("0.40.5")));
        final Path base = maven.targetPath().resolve(MjParse.DIR);
        final Path target = new Place("foo.x.main").make(base, MjAssemble.XMIR);
        new FpDefault(
            src -> expected,
            cache.resolve(MjParse.CACHE),
            FakeMaven.pluginVersion(),
            hash.value(),
            base.relativize(target)
        ).apply(maven.programTojo().source(), target);
        target.toFile().delete();
        Files.setLastModifiedTime(
            cache.resolve(
                Paths
                    .get(MjParse.CACHE)
                    .resolve(FakeMaven.pluginVersion())
                    .resolve(hash.value())
                    .resolve("foo/x/main.xmir")
            ),
            FileTime.fromMillis(System.currentTimeMillis() + 50_000)
        );
        final String actual = String.format(
            "target/%s/foo/x/main.%s",
            MjParse.DIR,
            MjAssemble.XMIR
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
    void doesNotCrashesOnError(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "Even if the eo program invalid we still have to parse it, but we didn't",
            new FakeMaven(temp)
                .withProgram("something < is wrong here")
                .execute(new FakeMaven.Parse())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", MjParse.DIR, MjAssemble.XMIR)
            )
        );
    }

    @Test
    void doesNotParseIfAlreadyParsed(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> result = maven
            .withHelloWorld()
            .execute(new FakeMaven.Parse())
            .result();
        final File parsed = result.get(
            String.format("target/%s/foo/x/main.%s", MjParse.DIR, MjAssemble.XMIR)
        ).toFile();
        final long before = parsed.lastModified();
        maven.execute(MjParse.class);
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
    void parsesConcurrentlyWithLotsOfPrograms(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int total = 50;
        for (int program = 0; program < total; ++program) {
            maven.withHelloWorld();
        }
        final Map<String, Path> res = maven.execute(new FakeMaven.Parse()).result();
        for (int program = 0; program < total; ++program) {
            MatcherAssert.assertThat(
                "We have to parse concurrently, but we didn't",
                res,
                Matchers.hasKey(
                    String.format(
                        "target/%s/foo/x/main%s.%s",
                        MjParse.DIR,
                        FakeMaven.suffix(program),
                        MjAssemble.XMIR
                    ))
            );
        }
    }

    /**
     * The mojo that does nothing, but executes infinitely.
     * @since 0.29
     */
    @Mojo(name = "infinite", defaultPhase = LifecyclePhase.VALIDATE)
    private static final class Infinite extends MjSafe {
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
