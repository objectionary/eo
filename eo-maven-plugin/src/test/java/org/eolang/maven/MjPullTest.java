/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.tojos.MnCsv;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.LinkedList;
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.scalar.ScalarOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjPull}.
 *
 * @since 0.1
 * @checkstyle ClassFanOutComplexityCheck (1000 lines)
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
final class MjPullTest {

    @Test
    void pullsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(this.stdout())
            .withVersion("*.*.*");
        maven.with("skip", false).execute(MjPull.class);
        MatcherAssert.assertThat(
            "PullMojo should have pulled stdout object, but didn't",
            MjPullTest.exists(temp, this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    void pullsFromProbes(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                "+package foo.x\n",
                "# No comments.",
                "[] > main",
                "  QQ.io.stdout > @",
                "    \"I am 18 years old\""
            )
            .with("objectionary", new ScalarOf<>(() -> new OyRemote(new ChRemote("master"))))
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            "PullMojo should have pulled from probes, but it didn't",
            MjPullTest.exists(temp, this.stdout()),
            Matchers.is(true)
        );
    }

    @Test
    void pullsUsingOfflineHashFile(@Mktmp final Path temp) throws IOException {
        new Saved(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            temp.resolve("tags.txt")
        ).value();
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(this.stdout())
            .withVersion("*.*.*");
        maven.with("skip", false)
            .with(
                "hash",
                new ChCached(new ChText(temp.resolve("tags.txt"), "master"))
            )
            .execute(MjPull.class);
        MatcherAssert.assertThat(
            "PullMojo should have pulled using offline hash file, but it didn't",
            new LinkedList<>(new MnCsv(maven.foreignPath()).read()).getFirst().get("hash"),
            Matchers.equalTo("mmmmmmm")
        );
    }

    @Test
    void pullsUsingOfflineHash(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(this.stdout())
            .withVersion("*.*.*");
        maven.with("skip", false)
            .with(
                "hash",
                new ChCached(new ChPattern("*.*.*:abcdefg", "1.0.0"))
            )
            .execute(MjPull.class);
        MatcherAssert.assertThat(
            "PullMojo should have pulled using offline hash, but it didn't",
            new LinkedList<>(new MnCsv(maven.foreignPath()).read()).getFirst().get("hash"),
            Matchers.equalTo("abcdefg")
        );
    }

    @Test
    void skipsPullMojo(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(this.stdout())
            .withScope("compile")
            .withVersion("*.*.*");
        maven.with("skip", true)
            .execute(MjPull.class);
        MatcherAssert.assertThat(
            "PullMojo must skip pulling, but it doesn't",
            MjPullTest.exists(temp, this.stdout()),
            Matchers.is(false)
        );
    }

    @Test
    void doesNotPullInOfflineMode(@Mktmp final Path tmp) throws IOException {
        final Map<String, Path> result = new FakeMaven(tmp)
            .withHelloWorld()
            .with("offline", true)
            .execute(new FakeMaven.Pull())
            .result();
        final String stdout = "org/eolang/io/stdout.eo";
        final String string = "org/eolang/string.eo";
        MatcherAssert.assertThat(
            String.format(
                "%s folder should not contain %s and %s file, but it did",
                MjPull.DIR,
                stdout,
                string
            ),
            result,
            Matchers.allOf(
                Matchers.not(Matchers.hasKey(String.format("%s/%s", MjPull.DIR, stdout))),
                Matchers.not(Matchers.hasKey(String.format("%s/%s", MjPull.DIR, string)))
            )
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void skipsAlreadyPulled(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Pull());
        final Path path = maven.result().get(
            String.format("target/%s/org/eolang/bytes.%s", MjPull.DIR, MjAssemble.EO)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(MjPull.class);
        MatcherAssert.assertThat(
            "PullMojo must skip pulling if source was already downloaded",
            path.toFile().lastModified(),
            Matchers.is(mtime)
        );
    }

    @Test
    void savesPulledResultsToCache(@Mktmp final Path temp) throws IOException {
        final Path cache = temp.resolve("cache");
        final CommitHash hash = new ChCached(
            new ChNarrow(
                new ChRemote("master")
            )
        );
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache.toFile())
            .with("hash", hash)
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            "Pulled results must be saved to cache",
            cache.resolve(MjPull.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash.value())
                .resolve("org/eolang/bytes.eo")
                .toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void getsAlreadyPulledResultsFromCache(@Mktmp final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        final String cached = "# test.\n[] > just-something\n";
        new Saved(
            cached,
            cache
                .resolve(MjPull.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("org/eolang/io/stdout.eo")
        ).value();
        Files.setLastModifiedTime(
            cache.resolve(
                Paths
                    .get(MjPull.CACHE)
                    .resolve(FakeMaven.pluginVersion())
                    .resolve(hash)
                    .resolve("org/eolang/io/stdout.eo")
            ),
            FileTime.fromMillis(System.currentTimeMillis() + 50_000)
        );
        new FakeMaven(temp)
            .withProgram(
                "+package foo.x",
                "# No comments.",
                "[] > main",
                "  QQ.io.stdout > @"
            )
            .with("hash", new CommitHash.ChConstant(hash))
            .with("cache", cache.toFile())
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            "PullMojo should take source from cache, but it does not",
            new TextOf(
                Files.readAllBytes(
                    temp.resolve(
                        String.format(
                            "target/%s/org/eolang/io/stdout.%s",
                            MjPull.DIR,
                            MjAssemble.EO
                        )
                    )
                )
            ).asString(),
            Matchers.is(cached)
        );
    }

    /**
     * Returns the stdout path.
     */
    private String stdout() {
        return "org.eolang.io.stdout";
    }

    /**
     * Check if the given source file exists in the target directory.
     *
     * @param temp Test temporary directory.
     * @param source Source file.
     * @return If given source file exists.
     */
    private static boolean exists(final Path temp, final String source) {
        return Files.exists(temp.resolve("target").resolve(MjPullTest.path(source)));
    }

    /**
     * Format given a source path.
     * @param source Source path as object name.
     * @return Formatted source path.
     */
    private static Path path(final String source) {
        return new Place(source).make(Paths.get(MjPull.DIR), "eo");
    }

}
