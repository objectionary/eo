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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.apache.log4j.Appender;
import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.cactoos.io.ResourceOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
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
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjLintTest {

    @Test
    void doesNotFailWithNoErrorsAndWarnings(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp)
            .withHelloWorld()
            .execute(new PpLint());
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withHelloWorld()
                .execute(new PpLint()),
            "Correct program should not have failed, but it does"
        );
    }

    @Test
    void includesDefectDetailsInExceptionMessage(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(MjLintTest.erroneous());
        final IllegalStateException thrown = Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
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
                Matchers.containsString("(unique-metas/S)")
            )
        );
    }

    @Test
    void detectsErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(MjLintTest.erroneous());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
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
    void reportsExperimentalDefectWhenSkipExperimentalIsFalse(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("skipExperimental", false).withProgram(
                "+architect yegor256@gmail.com",
                "+home https://www.eolang.org",
                "+package foo.x",
                "+version 0.0.0",
                "+unlint empty-object",
                "+unlint unit-test-missing",
                "+unlint comment-too-short",
                "+unlint object-has-data",
                "",
                "[] > main",
                "  (stdout \"Hello!\").print > @"
            );
        maven.execute(new PpLint());
        MatcherAssert.assertThat(
            "an experimental lint stays unreported while eo.skipExperimentalLints is FALSE",
            new Xnav(
                maven.programTojo().linted()
            ).path("/object/errors/error[@check='no-attribute-formation/S']").count(),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    void skipsExperimentalDefectWhenSkipExperimentalIsTrue(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("skipExperimental", true).withProgram(
                "+architect yegor256@gmail.com",
                "+home https://www.eolang.org",
                "+package foo.x",
                "+version 0.0.0",
                "+unlint empty-object",
                "+unlint unit-test-missing",
                "+unlint comment-too-short",
                "+unlint object-has-data",
                "",
                "[] > main",
                "  (stdout \"Hello!\").print > @"
            );
        maven.execute(new PpLint());
        MatcherAssert.assertThat(
            "an experimental lint is still reported while eo.skipExperimentalLints is TRUE",
            new Xnav(
                maven.programTojo().linted()
            ).path("/object/errors/error[@check='no-attribute-formation/S']").count(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void ignoresLintNamedInSkipSourceLints(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("skipSourceLints", new SetOf<>("mandatory-spdx")).withProgram(
                "+architect yegor256@gmail.com",
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
        maven.execute(new PpLint());
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
            "+architect yegor256@gmail.com",
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
            .execute(new PpLint());
        final FakeMaven second = new FakeMaven(temp)
            .with("cache", cache.toFile())
            .with("skipSourceLints", new SetOf<>("mandatory-spdx"))
            .withProgram(source)
            .allTojosWithHash(() -> "abcdefq");
        second.execute(new PpLint());
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
            .execute(new PpLint());
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
            .withProgram(MjLintTest.problematic());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
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
            .withProgram(MjLintTest.problematic());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
            "We should get WPA error here for the first time"
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
            "We should get WPA error here for the second time as well"
        );
    }

    @Test
    void namesTheProgramInAWholeProgramAnalysisDefectFromCache(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("lintAsPackage", true)
            .withProgram(MjLintTest.problematic());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
            "First (uncached) run must report the WPA error"
        );
        MatcherAssert.assertThat(
            "A WPA defect read back from the cache must still name its program, but it didn't",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> maven.execute(new PpLint()),
                "Second (cached) run must report the WPA error too"
            ).getCause().getCause().getMessage(),
            Matchers.containsString("foo.x.main")
        );
    }

    @Test
    void logsWholeProgramAnalysisDefectFromCache(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("lintAsPackage", true)
            .withProgram(MjLintTest.problematic());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
            "First (uncached) run must report the WPA error"
        );
        final List<String> messages = new ArrayList<>(0);
        final Appender appender = new AppenderSkeleton() {
            @Override
            protected void append(final LoggingEvent event) {
                messages.add(String.valueOf(event.getRenderedMessage()));
            }

            @Override
            public void close() {
                // Nothing to release.
            }

            @Override
            public boolean requiresLayout() {
                return false;
            }
        };
        final Logger logger = Logger.getLogger(Linting.class);
        final Level level = logger.getLevel();
        logger.setLevel(Level.ALL);
        logger.addAppender(appender);
        try {
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> maven.execute(new PpLint()),
                "Second (cached) run must report the WPA error too"
            );
        } finally {
            logger.removeAppender(appender);
            logger.setLevel(level);
        }
        MatcherAssert.assertThat(
            "A WPA defect read back from the cache must still be logged, but it wasn't",
            messages,
            Matchers.hasItem(
                Matchers.<String>allOf(
                    Matchers.containsString("[LINT]"),
                    Matchers.containsString("foo.x.main"),
                    Matchers.containsString("not in scope")
                )
            )
        );
    }

    @Test
    void savesForWholeProgramAnalysisResultsToCache(@Mktmp final Path temp) throws IOException {
        final Path cache = temp.resolve("wpa-cache");
        final FakeMaven maven = new FakeMaven(temp)
            .with("lintAsPackage", true)
            .allTojosWithHash(() -> "abcdefq")
            .with("cache", cache.toFile()).withProgram(
                "+architect yegor256@gmail.com",
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
        maven.execute(new PpLint());
        try (
            Stream<Path> walk = Files.walk(
                cache.resolve(Linting.CACHE).resolve(FakeMaven.pluginVersion())
            )
        ) {
            MatcherAssert.assertThat(
                "WPA results must be saved under the versioned cache path",
                walk.anyMatch(path -> "wpa.xmir".equals(path.getFileName().toString())),
                Matchers.is(true)
            );
        }
    }

    @Test
    void doesNotReuseWholeProgramAnalysisCacheOfAnotherProject(@Mktmp final Path temp)
        throws IOException {
        final Path cache = temp.resolve("shared-cache");
        MjLintTest.linting(temp.resolve("clean"), cache, MjLintTest.suppressed("foo.x", "main"))
            .execute(new PpLint());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> MjLintTest.linting(
                temp.resolve("broken"), cache, MjLintTest.problematic()
            ).execute(new PpLint()),
            "WPA error of the second project must be reported, not hidden by the cache of the first"
        );
    }

    @Test
    void invalidatesWholeProgramAnalysisCacheWhenCompileScopeChanges(@Mktmp final Path temp)
        throws IOException {
        final Path cache = temp.resolve("cache");
        final Path workspace = temp.resolve("project");
        final FakeMaven maven = new FakeMaven(workspace)
            .with("lintAsPackage", true)
            .with("cache", cache.toFile())
            .with("scope", "compile");
        maven.withProgram(MjLintTest.suppressed("foo.x", "main")).execute(MjParse.class);
        maven.with("scope", "test").withProgram(
            String.join(
                System.lineSeparator(),
                MjLintTest.suppressed("foo.x", "tests")
            ),
            "foo.x.tests",
            "foo/x/tests.eo"
        ).execute(new PpLint());
        new Saved(
            String.join(System.lineSeparator(), MjLintTest.problematic()),
            workspace.resolve("foo/x/main.eo")
        ).value();
        maven.with("scope", "compile").execute(MjParse.class);
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.with("scope", "test").execute(MjLint.class),
            "WPA must re-run when a compile-scope XMIR changes under a test-scope lint"
        );
    }

    @Test
    void lintsPackageWithoutASingleProgram(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> MjLintTest.linting(temp, temp.resolve("cache"))
                .execute(new PpLint()),
            "Linting a package that has no programs at all must not fail"
        );
    }

    @Test
    void ignoresWholeProgramAnalysisErrors(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .with("lintAsPackage", false)
                .withProgram(MjLintTest.problematic())
                .execute(new PpLint()),
            "We shouldn't get WPA error here because we disabled it with 'lintAsPackage' flag, but we got it"
        );
    }

    @Test
    void suppressesWholeProgramAnalysisDefectViaUnlint(@Mktmp final Path temp)
        throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("lintAsPackage", true)
            .with("failOnWarning", false).withProgram(
                "+architect yegor256@gmail.com",
                "+package foo.x",
                "+alias a.b.nowhere",
                "+unlint unused-alias",
                "+unlint incorrect-alias",
                "+unlint incorrect-unlint",
                "+unlint unlint-non-existing-defect",
                "",
                "[] > main"
            );
        maven.execute(new PpLint());
        MatcherAssert.assertThat(
            "A plain +unlint incorrect-alias must suppress the WPA defect reported as incorrect-alias/W",
            new Xnav(maven.programTojo().linted())
                .path("/object/errors/error[@check='incorrect-alias/W']").count(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void detectsErrorsSuccessfullyEvenAfterSecondRun(
        @Mktmp final Path temp
    ) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(MjLintTest.erroneous());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
            "Program with noname attributes should have failed or error for the first time, but it didn't"
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
            "We expect that even if the result was cached, we still get the same error"
        );
    }

    @Test
    void detectsCriticalErrorsSuccessfully(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(MjLintTest.critical());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
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
            () -> maven.execute(new PpLint()),
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
            String.format("+architect yegor256@gmail.com%n+package foo.x%n"),
            "[] > main",
            "  [] > @",
            "    \"Hello world\" > @"
            )
            .with("failOnWarning", true);
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
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
                String.format("+architect yegor256@gmail.com%n+package foo.x%n"),
                "[] > main",
                "  [] > x",
                "    \"Hello world\" > @"
                )
                .with("failOnWarning", false)
                .execute(new PpLint()),
            "Program with sparse decorated object should not have failed on warning without flag, but it does"
        );
    }

    @Test
    void failsParsingOnError(@Mktmp final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.format("+architect yegor256@gmail.com%n+package foo.x%n"),
            "[] > main",
            "  seq *-1 > @",
            "    true"
            );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
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
    void reportsPlainParserSyntaxErrorUnderParserRule(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "# Sample object for the probe.",
            "",
            "[] > main",
            "  \"\\uD800\" > @"
        );
        MatcherAssert.assertThat(
            "a plain parser syntax error must be reported as a defect under the 'parser' rule, the same as any other lint",
            new UncheckedText(
                new TextOf(
                    Assertions.assertThrows(
                        IllegalStateException.class,
                        () -> maven.execute(new PpLint()),
                        "A plain parser syntax error must still fail the build, but through the normal reporting path"
                    )
                )
            ).asString(),
            Matchers.containsString("(parser)")
        );
    }

    @Test
    void skipsAlreadyLinted(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .allTojosWithHash(CommitHash.FAKE)
            .execute(new PpLint());
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
            .execute(new PpLint());
        try (Stream<Path> saved = Files.walk(cache.resolve(Linting.CACHE))) {
            MatcherAssert.assertThat(
                "Verified results must be saved to cache, under the hash of the tojo",
                saved.filter(p -> p.endsWith(Paths.get(hash, "foo", "x", "main.xmir")))
                    .collect(Collectors.toList()),
                Matchers.hasSize(1)
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
            .execute(new PpLint());
        final String planted = new TextOf(new ResourceOf("org/eolang/maven/main.xml")).asString();
        try (Stream<Path> saved = Files.walk(cache.resolve(Linting.CACHE))) {
            saved.filter(p -> p.endsWith(Paths.get("foo", "x", "main.xmir")))
                .forEach(p -> new Unchecked<>(new Saved(planted, p)).value());
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
            () -> new FakeMaven(temp).withProgram("# App.").execute(new PpLint()),
            "MjLint's execution was not failed, but it should"
        );
    }

    @Test
    void failsOnUnusedAlias(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "+alias a.b.foo",
            "",
            "[] > main"
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new PpLint()),
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

    private static String[] critical() {
        return new String[]{
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > wrong",
            "  42 > @",
        };
    }

    private static String[] erroneous() {
        return new String[]{
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "+home https://www.eolang.org",
            "+home https://www.eolang.org",
            "",
            "[] > main",
            "  42 > @",
        };
    }

    private static FakeMaven linting(
        final Path workspace, final Path cache, final String... program
    ) throws IOException {
        final FakeMaven maven = new FakeMaven(workspace)
            .with("lintAsPackage", true)
            .with("cache", cache.toFile());
        if (program.length > 0) {
            maven.withProgram(program);
        }
        return maven;
    }

    private static String[] suppressed(final String pkg, final String name) {
        return new String[]{
            "+architect yegor256@gmail.com",
            "+home https://www.eolang.org",
            String.format("+package %s", pkg),
            "+version 0.0.0",
            "+unlint empty-object",
            "+unlint unit-test-missing",
            "+unlint mandatory-spdx",
            "+unlint comment-too-short",
            "+unlint object-has-data",
            "",
            String.format("[] > %s", name),
        };
    }

    private static String[] problematic() {
        return new String[]{
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "+alias a.b.nowhere",
            "+unlint unused-alias",
            "",
            "[] > main",
        };
    }
}
