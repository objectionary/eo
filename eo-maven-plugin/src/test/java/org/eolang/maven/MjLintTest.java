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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjLint}.
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
    @SuppressWarnings({"PMD.UnitTestContainsTooManyAsserts", "PMD.UnnecessaryLocalRule"})
    void includesDefectDetailsInExceptionMessage(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "# Main object.",
            "",
            String.format("+package foo.x%n"),
            "[] > main",
            "  cti true \"error\" \"msg\" > @"
            );
        final IllegalStateException thrown = Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Program with errors should have failed, but it didn't"
        );
        Throwable root = thrown;
        while (root.getCause() != null) {
            root = root.getCause();
        }
        MatcherAssert.assertThat(
            "Root exception message must include the program name so defects are visible in stacktrace",
            root.getMessage(),
            Matchers.containsString("foo.x.main")
        );
        MatcherAssert.assertThat(
            "Root exception message must include the failing rule in parentheses",
            root.getMessage(),
            Matchers.containsString("(cti)")
        );
    }

    @Test
    void detectsErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "# Main object.",
            "",
            String.format("+package foo.x%n"),
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
    void detectsWholeProgramAnalysisErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("lintAsPackage", true)
            .withProgram(MjLintTest.probmlematic());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "We should get WPA error here: 'Alias \"nowhere Φ.a.b.nowhere\" points to \"a.b.nowhere\", but it's not in scope (1): [\"foo.x.main\"]'"
        );
        MatcherAssert.assertThat(
            "We don't add critical errors to XMIR and throw exception instead",
            new Xnav(
                maven.programTojo().linted()
            ).path("/object/errors/error[@severity='critical']").count(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void detectsWholeProgramAnalysisErrorsOnSecondRun(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("lintAsPackage", true)
            .withProgram(MjLintTest.probmlematic());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "We should get WPA error here for the first time"
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "We should get WPA error here for the second time as well"
        );
    }

    @Test
    void savesForWholeProgramAnalysisResultsToCache(@Mktmp final Path temp) throws IOException {
        final Path cache = temp.resolve("wpa-cache");
        final FakeMaven maven = new FakeMaven(temp)
            .with("lintAsPackage", true)
            .allTojosWithHash(() -> "abcdefq")
            .with("cache", cache.toFile()).withProgram(
                "+home https://www.eolang.org",
                "+package foo.x",
                "+version 0.0.0",
                "+unlint empty-object",
                "+unlint unit-test-missing",
                "+unlint mandatory-spdx",
                "+unlint comment-too-short",
                "+unlint object-has-data",
                "",
                "[x] > main",
                "  (stdout \"Hello!\" x).print > @"
            );
        maven.execute(new FakeMaven.Lint());
        MatcherAssert.assertThat(
            "WPA results must be saved to cache",
            cache.resolve(Linting.CACHE)
                .resolve("wpa.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void ignoresWholeProgramAnalysisErrors(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .with("lintAsPackage", false)
                .withProgram(MjLintTest.probmlematic())
                .execute(new FakeMaven.Lint()),
            "We shouldn't get WPA error here because we disabled it with 'lintAsPackage' flag, but we got it"
        );
    }

    @Test
    void detectsErrorsSuccessfullyEvenAfterSecondRun(
        @Mktmp final Path temp
    ) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.format("+package foo.x%n"),
            "[] > main",
            "  cti true \"error\" \"msg\" > @"
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Program with noname attributes should have failed or error for the first time, but it didn't"
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "We expect that even if the result was cached, we still get the same error"
        );
    }

    @Test
    @SuppressWarnings("PMD.UnitTestContainsTooManyAsserts")
    void detectsCriticalErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "# Main object.",
            "",
            String.format("+package foo.x%n"),
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
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.format("+package foo.x%n"),
            "[] > main",
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
            () -> new FakeMaven(temp).withProgram(
                String.format("+package foo.x%n"),
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
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.format("+package foo.x%n"),
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
            "Parser errors must exist in the parsed XMIR (the lint stage aborts before producing a linted file when the source has parse errors)",
            new Xnav(maven.programTojo().xmir()).path(
                "/object/errors/error[@severity='error']"
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
            String.format("target/%s/foo/x/main.%s", Linting.DIR, MjAssemble.XMIR)
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
            cache.resolve(Linting.CACHE)
                .resolve(FakeMaven.pluginVersion())
                .resolve(hash)
                .resolve("foo/x/main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Disabled
    @Test
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
                cache.resolve(Linting.CACHE),
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
                            Linting.DIR,
                            MjAssemble.XMIR
                        )
                    )
                )
            ),
            Matchers.is(
                new XMLDocument(
                    cache.resolve(Linting.CACHE)
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

    @Test
    void failsOnUnusedAlias(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "+package foo.x",
            "+alias a.b.foo",
            "",
            "[] > main"
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Linting must fail on an unused alias"
        );
        MatcherAssert.assertThat(
            "Linted XMIR must report the unused alias by name",
            new Xnav(maven.programTojo().linted())
                .one("/object/errors/error[@severity='error']")
                .text()
                .orElseThrow(),
            Matchers.stringContainsInOrder("a.b.foo", "is not used")
        );
    }

    /**
     * Program with WPA error.
     * @return Program with WPA error
     */
    private static String[] probmlematic() {
        return new String[]{
            "+package foo.x",
            "+alias a.b.nowhere",
            "+unlint unused-alias",
            "",
            "[] > main",
        };
    }
}
