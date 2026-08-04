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
import java.util.Collection;
import java.util.HashSet;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.io.ResourceOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjLint}.
 * @since 0.31.0
 */
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
    void includesDefectDetailsInExceptionMessage(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "",
                "[] > main",
                "  cti true \"error\" \"msg\" > @"
            )
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
            "Root exception message must name the program and the failing rule, so defects are visible in stacktrace",
            root.getMessage(),
            Matchers.allOf(
                Matchers.containsString("foo.x.main"),
                Matchers.containsString("(cti)")
            )
        );
    }

    @Test
    void detectsErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "",
                "[] > main",
                "  cti true \"error\" \"msg\" > @"
            )
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
    void ignoresLintNamedInSkipSourceLints(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("skipSourceLints", new SetOf<>("mandatory-spdx")).withProgram(
                "+home https://www.eolang.org",
                "+package foo.x",
                "+version 0.0.0",
                "+unlint empty-object",
                "+unlint unit-test-missing",
                "+unlint comment-too-short",
                "+unlint object-has-data",
                "",
                "[x] > main",
                "  (stdout \"Hello!\" x).print > @"
            );
        maven.execute(new FakeMaven.Lint());
        MatcherAssert.assertThat(
            "the lint named in eo.skipSourceLints is still reported",
            new Xnav(
                maven.programTojo().linted()
            ).path("/object/errors/error[@check='mandatory-spdx/S']").count(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void doesNotReuseStaleLintCacheAfterSkipSourceLintsChanges(@Mktmp final Path temp)
        throws IOException {
        final Path cache = temp.resolve("lint-cache");
        final String[] source = {
            "+home https://www.eolang.org",
            "+package foo.x",
            "+version 0.0.0",
            "+unlint empty-object",
            "+unlint unit-test-missing",
            "+unlint comment-too-short",
            "+unlint object-has-data",
            "",
            "[x] > main",
            "  (stdout \"Hello!\" x).print > @",
        };
        new FakeMaven(temp)
            .with("cache", cache.toFile())
            .withProgram(source)
            .allTojosWithHash(() -> "abcdefq")
            .execute(new FakeMaven.Lint());
        final FakeMaven second = new FakeMaven(temp)
            .with("cache", cache.toFile())
            .with("skipSourceLints", new SetOf<>("mandatory-spdx"))
            .withProgram(source)
            .allTojosWithHash(() -> "abcdefq");
        second.execute(new FakeMaven.Lint());
        MatcherAssert.assertThat(
            "changing skipSourceLints must invalidate the stale per-file lint cache",
            new Xnav(second.programTojo().linted())
                .path("/object/errors/error[@check='mandatory-spdx/S']").count(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void keepsLintCachePathSegmentShortWithManySkippedLints(@Mktmp final Path temp)
        throws IOException {
        final Path cache = temp.resolve("lint-cache");
        final Collection<String> skipped = new HashSet<>(0);
        for (int idx = 0; idx < 20; ++idx) {
            skipped.add(String.format("unlint-non-existing-defect-number-%d", idx));
        }
        new FakeMaven(temp)
            .with("cache", cache.toFile())
            .with("skipSourceLints", skipped)
            .withHelloWorld()
            .execute(new FakeMaven.Lint());
        try (Stream<Path> versions = Files.list(cache.resolve(Linting.CACHE))) {
            MatcherAssert.assertThat(
                "a lint cache-key path segment must stay well under the 255-byte limit ext4 and APFS enforce, no matter how many lints are skipped",
                versions.map(p -> p.getFileName().toString().length())
                    .collect(Collectors.toList()),
                Matchers.everyItem(Matchers.lessThan(255))
            );
        }
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
                .resolve(FakeMaven.pluginVersion())
                .resolve("wpa.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void doesNotReuseWholeProgramAnalysisCacheOfAnotherProject(@Mktmp final Path temp)
        throws IOException {
        final Path cache = temp.resolve("shared-cache");
        MjLintTest.linting(temp.resolve("clean"), cache, MjLintTest.flawless())
            .execute(new FakeMaven.Lint());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> MjLintTest.linting(
                temp.resolve("broken"), cache, MjLintTest.probmlematic()
            ).execute(new FakeMaven.Lint()),
            "WPA error of the second project must be reported, not hidden by the cache of the first"
        );
    }

    @Test
    void lintsPackageWithoutASingleProgram(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .with("lintAsPackage", true)
                .with("cache", temp.resolve("cache").toFile())
                .execute(new FakeMaven.Lint()),
            "Linting a package that has no programs at all must not fail"
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
            String.join(
                System.lineSeparator(),
                "+package foo.x",
                "",
                "[] > main",
                "  cti true \"error\" \"msg\" > @"
            )
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
    void detectsCriticalErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(MjLintTest.critical());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Wrong program should have failed or error, but it didn't"
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
    void keepsLintedFileWhenCriticalErrorFound(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(MjLintTest.critical());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Lint()),
            "Wrong program should have failed or error, but it didn't"
        );
        MatcherAssert.assertThat(
            "Linted file must stay on disk even when a critical error is found, but it didn't",
            maven.programTojo().linted().toFile(),
            FileMatchers.anExistingFile()
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

    @Test
    void savesVerifiedResultsToCache(@Mktmp final Path temp) throws IOException {
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache.toFile())
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Lint());
        try (Stream<Path> saved = Files.walk(cache.resolve(Linting.CACHE))) {
            MatcherAssert.assertThat(
                "Verified results must be saved to cache, under the hash of the tojo",
                saved.map(Path::toString).collect(Collectors.toList()),
                Matchers.hasItem(
                    Matchers.endsWith(
                        Paths.get(hash, "foo", "x", "main.xmir").toString()
                    )
                )
            );
        }
    }

    @Test
    void getsAlreadyVerifiedResultsFromCache(@Mktmp final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .with("cache", cache.toFile())
            .allTojosWithHash(() -> "abcdef1")
            .execute(new FakeMaven.Lint());
        final String planted = new TextOf(
            new ResourceOf("org/eolang/maven/main.xml")
        ).asString();
        try (Stream<Path> saved = Files.walk(cache.resolve(Linting.CACHE))) {
            new Saved(
                planted,
                saved.filter(p -> p.endsWith(Paths.get("foo", "x", "main.xmir")))
                    .findFirst()
                    .orElseThrow(
                        () -> new IllegalStateException(
                            String.format("nothing was cached under %s", cache)
                        )
                    )
            ).value();
        }
        maven.execute(MjLint.class);
        MatcherAssert.assertThat(
            "We must get already verified results from cache",
            new XMLDocument(maven.programTojo().linted()),
            Matchers.is(new XMLDocument(planted))
        );
    }

    @Test
    void reportsWhenObjectNameFails(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
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
     * Program with a critical defect.
     * @return Program with a critical defect
     */
    private static String[] critical() {
        return new String[]{
            "+package foo.x",
            "",
            "[] > main",
            "  cti true \"critical\" \"msg\" > @",
        };
    }

    /**
     * Maven that lints a package, keeping its EO sources apart from its target directory,
     * the way a real Maven build does with {@code src/main/eo}.
     * @param workspace Workspace of the project
     * @param cache Cache directory, possibly shared with other projects
     * @param program Program to lint
     * @return Maven ready to be executed
     * @throws IOException If fails to save the program
     */
    private static FakeMaven linting(
        final Path workspace, final Path cache, final String... program
    ) throws IOException {
        return new FakeMaven(workspace)
            .with("lintAsPackage", true)
            .with("cache", cache.toFile())
            .with("sourcesDir", workspace.resolve("foo").toFile())
            .withProgram(program);
    }

    /**
     * Program without a single defect.
     * @return Program without defects
     */
    private static String[] flawless() {
        return new String[]{
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
            "  (stdout \"Hello!\" x).print > @",
        };
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
