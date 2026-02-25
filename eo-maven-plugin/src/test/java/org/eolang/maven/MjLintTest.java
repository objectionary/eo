/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjLintTest {
    @Test
    void doesNotFailWithNoErrorsAndWarnings(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Lint());
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .execute(new FakeMaven.Lint()),
            "Correct program should not have failed, but it does"
        );
    }

    @Test
    @SuppressWarnings({
        "PMD.UnitTestContainsTooManyAsserts",
        "PMD.UnnecessaryLocalRule"
    })
    void detectsErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package foo.x\n",
                "# No comments.",
                "[] > main",
                "  cti true \"error\" \"msg\" > @"
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Program with noname attributes should have failed or error, but it didn't"
        );
        MatcherAssert.assertThat(
            "Critical errors must exist in linted XMIR",
            new Xnav(
                maven.programTojo().linted()
            ).path("/object/errors/error[@severity='error']").count(),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    @SuppressWarnings("PMD.UnitTestContainsTooManyAsserts")
    void detectsCriticalErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package foo.x\n",
                "# No comments.",
                "[] > main",
                "  cti true \"critical\" \"msg\" > @"
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Wrong program should have failed or error, but it didn't"
        );
        MatcherAssert.assertThat(
            "Linted file should not exist",
            maven.programTojo().linted().toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            "Error must exist in parsed XMIR",
            new Xnav(
                new XMLDocument(maven.programTojo().xmir()).inner()
            ).path("//errors/error[@severity='critical']").count(),
            Matchers.equalTo(1L)
        );
    }

    @Test
    @SuppressWarnings({
        "PMD.UnitTestContainsTooManyAsserts",
        "PMD.UnnecessaryLocalRule"
    })
    void detectsWarningWithCorrespondingFlag(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package foo.x\n",
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
                maven.programTojo().linted()
            ).path("/object/errors/error[@severity='warning']").count(),
            Matchers.greaterThanOrEqualTo(2L)
        );
    }

    @Test
    void doesNotDetectWarningWithoutCorrespondingFlag(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package foo.x\n",
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
    @SuppressWarnings({
        "PMD.UnitTestContainsTooManyAsserts",
        "PMD.UnnecessaryLocalRule"
    })
    void failsParsingOnError(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package foo.x\n",
                "# No comments.",
                "[] > main",
                "  seq *-1 > @",
                "    true"
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Program with invalid syntax should have failed, but it didn't"
        );
        MatcherAssert.assertThat(
            "Parsing errors must exist in linted XMIR",
            new Xnav(maven.programTojo().linted()).path(
                "/object/errors/error[@severity='critical' and @check='eo-parser']"
            ).count(),
            Matchers.greaterThan(0L)
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
        final String xpath = "/object/@time";
        final String before = new Xnav(path).one(xpath).text().orElseThrow();
        maven.execute(MjLint.class);
        final String after = new Xnav(path).one(xpath).text().orElseThrow();
        MatcherAssert.assertThat(
            String.format(
                "must skip verification if XMIR was already verified, we check it by time attribute in XMIR (before: '%s', after: '%s'), but it was changed",
                before, after
            ),
            before,
            Matchers.equalTo(after)
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
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
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void getsAlreadyVerifiedResultsFromCache(@Mktmp final Path temp) throws Exception {
        final TextOf input = new TextOf(
            new ResourceOf("org/eolang/maven/main.xml")
        );
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        final Path from = temp.resolve("input.xml");
        new Saved(input, from).value();
        new Cache(
            new CachePath(
                cache.resolve(MjLint.CACHE),
                FakeMaven.pluginVersion(),
                hash
            ),
            p -> input.asString()
        ).apply(from, temp.resolve("main.xmir"), Paths.get("foo/x/main.xmir"));
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache.toFile())
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Lint());
        MatcherAssert.assertThat(
            "We must get already verified results from cache",
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
            Matchers.is(
                new XMLDocument(
                    cache.resolve(MjLint.CACHE)
                        .resolve(FakeMaven.pluginVersion())
                        .resolve(hash)
                        .resolve("foo/x/main.xmir")
                )
            )
        );
    }

    @Test
    void reportsWhenObjectNameFails(@Mktmp final Path temp) {
        Assertions.assertThrows(
            Exception.class,
            () -> new FakeMaven(temp).withProgram("# App.").execute(new FakeMaven.Lint()),
            "MjLint's execution was not failed, but it should"
        );
    }
}
