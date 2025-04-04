/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjLint}.
 *
 * @since 0.31.0
 * @todo #4049:30min Replace all occurrences of new XMLDocument().nodes() with new Xnav().path().
 *  Right now we don't use {@link XMLDocument#nodes(String)} and {@link XMLDocument#xpath(String)}
 *  in production code, we got rid of it and replaced with {@link Xnav#path(String)} and
 *  {@link Xnav#element(String)}. But we didn't do it in the tests. Let's do it, it should increase
 *  the performance of our tests and make our code more consistent.
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjLintTest {
    @Test
    void doesNotFailWithNoErrorsAndWarnings(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .execute(new FakeMaven.Lint()),
            "Correct program should not have failed, but it does"
        );
    }

    @Test
    void detectsErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package f\n",
                "# No comments.",
                "[] > main",
                "  QQ.io.stdout",
                "    \"Hello world\""
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Program with noname attributes should have failed or error, but it didn't"
        );
        final Path xmir = maven.result().get(
            String.format("target/%s/foo/x/main.xmir", MjLint.DIR)
        );
        MatcherAssert.assertThat(
            "Linted file should exist",
            xmir,
            Matchers.not(Matchers.equalTo(null))
        );
        MatcherAssert.assertThat(
            "Critical error must exist in linted XMIR",
            new Xnav(xmir).path("/program/errors/error[@severity='error']").count(),
            Matchers.equalTo(1L)
        );
    }

    @Test
    void detectsCriticalErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package f\n",
                "# No comments.",
                "[] > main",
                "    \"Hello world\""
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Wrong program should have failed or error, but it didn't"
        );
        MatcherAssert.assertThat(
            "Linted file should not exist",
            maven.result(),
            Matchers.hasKey(String.format("target/%s/foo/x/main.xmir", MjLint.DIR))
        );
        MatcherAssert.assertThat(
            "Error must exist in parsed XMIR",
            new Xnav(
                new XMLDocument(
                    maven.result().get(String.format("target/%s/foo/x/main.xmir", MjParse.DIR))
                ).inner()
            ).path("//errors/error[@severity='critical']").count(),
            Matchers.equalTo(3L)
        );
    }

    @Test
    void detectsWarningWithCorrespondingFlag(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package f\n",
                "# No comments.",
                "[] > main",
                "  # No comments.",
                "  [] > @",
                "    \"Hello world\" > @"
            )
            .with("failOnWarning", true);
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Program with sparse decorated object should have failed on warning, but it didn't"
        );
        MatcherAssert.assertThat(
            "Warning must exist in shaken XMIR",
            new Xnav(
                maven.result().get(String.format("target/%s/foo/x/main.xmir", MjLint.DIR))
            ).path("//errors/error[@severity='warning']").count(),
            Matchers.greaterThanOrEqualTo(2L)
        );
    }

    @Test
    void doesNotDetectWarningWithoutCorrespondingFlag(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f",
                    "+unlint object-has-data\n",
                    "# No comments.",
                    "[] > main",
                    "  [] > x",
                    "    \"Hello world\" > @"
                )
                .with("failOnWarning", false)
                .execute(new FakeMaven.Lint()),
            "Program with sparse decorated object should not have failed on warning without flag, but it does"
        );
    }

    @Test
    void failsOptimizationOnError(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f",
                    "+alias THIS-IS-WRONG org.eolang.io.stdout\n",
                    "# No comments.",
                    "[args] > main",
                    "  (stdout \"Hello!\").print > @"
                )
                .execute(new FakeMaven.Lint()),
            "Error in the eo code because of invalid alias, should fail"
        );
    }

    @Test
    void failsOptimizationOnCritical(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "# No comments.",
                    "[args] > main",
                    "  seq > @",
                    "    TRUE > x",
                    "    FALSE > x"
                ).with("trackTransformationSteps", true)
                .execute(new FakeMaven.Lint()),
            "Program should have failed, but it didn't"
        );
    }

    @Test
    void failsParsingOnError(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram("something > is wrong here")
                .execute(new FakeMaven.Lint()),
            "Program with invalid syntax should have failed, but it didn't"
        );
    }

    @Test
    void failsOnInvalidProgram(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+alias stdout org.eolang.io.stdout",
                    "+home https://github.com/objectionary/eo",
                    "+package test",
                    "+version 0.0.0",
                    "",
                    "[x] < wrong>",
                    "  (stdout \"Hello!\" x).print"
                )
                .execute(new FakeMaven.Lint()),
                "Invalid program with wrong syntax should have failed to assemble, but it didn't"
        );
    }

    @Test
    void failsOnWarning(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+architect yegor256@gmail.com",
                    "+tests",
                    "+package org.eolang.examples\n",
                    "# No comments.",
                    "[] > main",
                    "  [] > @",
                    "    hello > test"
                )
                .with("failOnWarning", true)
                .execute(new FakeMaven.Lint()),
            "Program with warning should fail"
        );
    }

    @Test
    void skipsAlreadyLinted(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .allTojosWithHash(CommitHash.FAKE)
            .execute(new FakeMaven.Lint());
        final Path path = maven.result().get(
            String.format("target/%s/foo/x/main.%s", MjLint.DIR, MjAssemble.XMIR)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(MjLint.class);
        MatcherAssert.assertThat(
            "VerifyMojo must skip verification if XMIR was already verified",
            path.toFile().lastModified(),
            Matchers.is(mtime)
        );
    }

    @Test
    void savesVerifiedResultsToCache(@Mktmp final Path temp) throws IOException {
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache.toFile())
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Lint());
        MatcherAssert.assertThat(
            "Verified results must be saved to cache",
            cache.resolve(MjLint.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void getsAlreadyVerifiedResultsFromCache(@Mktmp final Path temp) throws Exception {
        final TextOf cached = new TextOf(
            new ResourceOf("org/eolang/maven/main.xml")
        );
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new Saved(
            cached,
            cache
                .resolve(MjLint.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir")
        ).value();
        Files.setLastModifiedTime(
            cache.resolve(
                Paths
                    .get(MjLint.CACHE)
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
            .execute(new FakeMaven.Lint());
        MatcherAssert.assertThat(
            "Cached result should match the original verified XML document",
            new XMLDocument(
                Files.readAllBytes(
                    temp.resolve(
                        String.format(
                            "target/%s/foo/x/main.%s",
                            MjLint.DIR,
                            MjAssemble.XMIR
                        )
                    )
                )
            ),
            Matchers.is(new XMLDocument(cached.asString()))
        );
    }
}
