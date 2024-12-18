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

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import com.yegor256.tojos.MnCsv;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.LinkedList;
import java.util.Map;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.footprint.Saved;
import org.eolang.maven.hash.ChCached;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChPattern;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.hash.ChText;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.log.CaptureLogs;
import org.eolang.maven.log.Logs;
import org.eolang.maven.objectionary.OyRemote;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link PullMojo}.
 *
 * @since 0.1
 * @checkstyle ClassFanOutComplexityCheck (1000 lines)
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(WeAreOnline.class)
@ExtendWith(MktmpResolver.class)
final class PullMojoTest {
    /**
     * Stdout.
     */
    private static final String STDOUT = "org.eolang.io.stdout";

    @Test
    void pullsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withVersion("*.*.*");
        maven.with("skip", false).execute(PullMojo.class);
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            PullMojoTest.exists(temp, PullMojoTest.STDOUT),
            Matchers.is(true)
        );
    }

    @Test
    void pullsFromProbes(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withProgram(
                "+package org.eolang.custom\n",
                "# No comments.",
                "[] > main",
                "  QQ.io.stdout > @",
                "    \"I am 18 years old\""
            )
            .with("objectionary", new OyRemote(new ChRemote("master")))
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            PullMojoTest.exists(temp, PullMojoTest.STDOUT),
            Matchers.is(true)
        );
    }

    @Test
    void pullsUsingOfflineHashFile(@Mktmp final Path temp) throws IOException {
        new HmBase(temp).save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            Paths.get("tags.txt")
        );
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withVersion("*.*.*");
        maven.with("skip", false)
            .with(
                "hash",
                new ChCached(new ChText(temp.resolve("tags.txt"), "master"))
            )
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new LinkedList<>(new MnCsv(maven.foreignPath()).read()).getFirst().get("hash"),
            Matchers.equalTo("mmmmmmm")
        );
    }

    @Test
    void pullsUsingOfflineHash(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withVersion("*.*.*");
        maven.with("skip", false)
            .with(
                "hash",
                new ChCached(new ChPattern("*.*.*:abcdefg", "1.0.0"))
            )
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            new LinkedList<>(new MnCsv(maven.foreignPath()).read()).getFirst().get("hash"),
            Matchers.equalTo("abcdefg")
        );
    }

    @Test
    void skipsPullMojo(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreignTojos()
            .add(PullMojoTest.STDOUT)
            .withScope("compile")
            .withVersion("*.*.*");
        maven.with("skip", true)
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            PullMojoTest.exists(temp, PullMojoTest.STDOUT),
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
        final String format = "%s folder should not contain %s file, but it did";
        final String stdout = "org/eolang/io/stdout.eo";
        final String string = "org/eolang/string.eo";
        MatcherAssert.assertThat(
            String.format(format, PullMojo.DIR, stdout),
            result.containsKey(String.format("%s/%s", PullMojo.DIR, stdout)),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            String.format(format, PullMojo.DIR, string),
            result.containsKey(String.format("%s/%s", PullMojo.DIR, string)),
            Matchers.is(false)
        );
    }

    @Test
    @CaptureLogs
    void showsWhereNotFoundWasDiscoveredAt(@Mktmp final Path tmp, final Logs out)
        throws IOException {
        final FakeMaven mvn = new FakeMaven(tmp)
            .withProgram(
                "+package com.example\n",
                "# No comments.",
                "[] > main",
                "  org.eolang.org > @"
            )
            .with("objectionary", new OyRemote(new ChRemote("master")));
        Assertions.assertThrows(
            Exception.class,
            () -> mvn.execute(new FakeMaven.Pull()),
            "Pull mojo should fail, but it does not"
        );
        Assertions.assertTrue(
            out.captured().stream().anyMatch(
                line -> line.contains("Failed to pull 'org.eolang.org' earlier discovered at")
            ),
            "Log should contain info where failed to pull object was discovered at, but it does not"
        );
    }

    @Test
    void skipsAlreadyPulled(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Pull());
        final Path path = maven.result().get(
            String.format("target/%s/org/eolang/bytes.%s", PullMojo.DIR, AssembleMojo.EO)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(PullMojo.class);
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
            cache.resolve(PullMojo.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash.value())
                .resolve("org/eolang/bytes.eo")
                .toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    @Disabled
    void ignoresPreviousMistakesAfterCorrection(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        "\n",
                        "+package org.eolang",
                        "",
                        "# In this program, we refer to the 'bar' object",
                        "# by mistake. The build should fail because of this,",
                        "# in particular its 'pull' step must fail.",
                        "[] > foo",
                        "  bar 42 > @",
                        ""
                    ).getBytes()
                );
                f.build().plugins().appendItself();
                f.exec("eo:register", "eo:parse", "eo:optimize", "eo:shake");
                MatcherAssert.assertThat(
                    "first run must fail, because the 'bar' object is absent",
                    f.execQuiet("eo:discover-foreign", "eo:pull"),
                    Matchers.not(Matchers.equalTo(0))
                );
                f.files().file("src/main/eo/foo.eo").write(
                    String.join(
                        "\n",
                        "+package org.eolang",
                        "",
                        "# Now, this program, doesn't refer to the 'bar' object",
                        "# which makes this program valid and it must compile.",
                        "[] > foo",
                        "  42 > @",
                        ""
                    ).getBytes()
                );
                f.exec(
                    "eo:register", "eo:parse", "eo:optimize", "eo:shake",
                    "eo:discover-foreign", "eo:pull"
                );
            }
        );
        MatcherAssert.assertThat(
            "necessary objects were pulled",
            temp.resolve("target/eo/4-pull/org/eolang/number.eo").toFile().exists(),
            Matchers.is(true)
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
                .resolve(PullMojo.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("org/eolang/io/stdout.eo")
        ).value();
        Files.setLastModifiedTime(
            cache.resolve(
                Paths
                    .get(PullMojo.CACHE)
                    .resolve(FakeMaven.pluginVersion())
                    .resolve(hash)
                    .resolve("org/eolang/io/stdout.eo")
            ),
            FileTime.fromMillis(System.currentTimeMillis() + 50_000)
        );
        new FakeMaven(temp)
            .withProgram(
                "# No comments.",
                "[] > app",
                "  QQ.io.stdout > @"
            )
            .with("hash", new CommitHash.ChConstant(hash))
            .with("cache", cache.toFile())
            .execute(new FakeMaven.Pull());
        MatcherAssert.assertThat(
            "PullMojo should take source from cache, but it does not",
            new TextOf(
                new HmBase(temp).load(
                    Paths.get(
                        String.format(
                            "target/%s/org/eolang/io/stdout.%s",
                            PullMojo.DIR,
                            AssembleMojo.EO
                        )
                    )
                ).asBytes()
            ).asString(),
            Matchers.is(cached)
        );
    }

    /**
     * Check if the given source file exists in the target directory.
     *
     * @param temp Test temporary directory.
     * @param source Source file.
     * @return If given source file exists.
     */
    private static boolean exists(final Path temp, final String source) {
        return new HmBase(temp.resolve("target")).exists(PullMojoTest.path(source));
    }

    /**
     * Format given a source path.
     * @param source Source path as object name.
     * @return Formatted source path.
     */
    private static Path path(final String source) {
        return new Place(source).make(Paths.get(PullMojo.DIR), "eo");
    }

}
