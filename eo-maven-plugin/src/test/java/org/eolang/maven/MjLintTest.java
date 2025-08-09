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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjLint}.
 *
 * @since 0.31.0
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjLintTest {
    @Disabled
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

    @Disabled
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

    @Disabled
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

    @Disabled
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

    @Test
    void reportsWhenObjectNameFails(@Mktmp final Path temp) {
        Assertions.assertThrows(
            Exception.class,
            () -> new FakeMaven(temp).withProgram("# App.").execute(new FakeMaven.Lint()),
            "MjLint's execution was not failed, but it should"
        );
    }
}
