/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.TrDefault;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link MjTranspile}.
 * @since 0.1
 */
@SuppressWarnings({
    "PMD.TooManyMethods",
    "PMD.UnitTestContainsTooManyAsserts"
})
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjTranspileTest {

    /**
     * Test eo program from resources.
     * @checkstyle ProhibitFieldsInTestClassesCheck (5 lines)
     */
    private String program;

    /**
     * Traspiled to java eo program from resources.
     * @checkstyle ProhibitFieldsInTestClassesCheck (5 lines)
     */
    private String compiled;

    @BeforeEach
    void setUp() throws Exception {
        this.program = new TextOf(new ResourceOf("org/eolang/maven/mess.eo")).asString();
        this.compiled = "target/generated/org/eolang/EO_foo/EO_x/EOmain.java";
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/transpile-packs/", glob = "**.yaml")
    void checksTranspilePacks(final String yaml) {
        final org.eolang.xax.Xtory story = new XtSticky(
            new XtYaml(
                yaml,
                eo -> new EoSyntax(
                    new InputOf(String.format("%s%n", eo))
                ).parsed(),
                new TrDefault<>()
            )
        );
        org.junit.jupiter.api.Assumptions.assumeTrue(story.map().get("skip") == null);
        MatcherAssert.assertThat(
            "passed without exceptions",
            story,
            new XtoryMatcher()
        );
    }

    @Test
    void transpilesSimpleProgram(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).withProgram(
                String.join(
                    System.lineSeparator(),
                    "+architect yegor256@gmail.com",
                    "+package examples",
                    "",
                    "[] > x"
                )
                ).with("trackTransformationSteps", true)
                .execute(MjParse.class)
                .execute(MjTranspile.class),
            "We should be able to transpile a simple EO program without exceptions when tracking transformation steps"
        );
    }

    @Test
    void wrapsObjectsIntoPhCoverageWhenFileIsSet(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must wrap located objects into PhCoverage when coverageFile is set",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(this.program)
                    .with("coverageFile", temp.resolve("hits.txt").toFile())
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(this.compiled)
            ).asString(),
            Matchers.containsString("new PhCoverage(")
        );
    }

    @Test
    void writesACoverageManifestWhenFileIsSet(@Mktmp final Path temp) throws Exception {
        final String manifest = new TextOf(
            new FakeMaven(temp)
                .withProgram(this.program)
                .with("coverageFile", temp.resolve("hits.txt").toFile())
                .execute(new FakeMaven.Transpile())
                .result()
                .get("hits.txt.manifest")
        ).asString();
        MatcherAssert.assertThat(
            String.join(
                " ",
                "the manifest must list every location wrapped into PhCoverage,",
                "one tab-separated locator/line entry per line"
            ),
            manifest.trim().split(System.lineSeparator()).length,
            Matchers.greaterThan(0)
        );
        for (final String line : manifest.trim().split(System.lineSeparator())) {
            MatcherAssert.assertThat(
                "each manifest line must have exactly two tab-separated columns",
                line.split("\t", -1).length,
                Matchers.equalTo(2)
            );
        }
    }

    @Test
    void keepsGeneratedJavaFreeOfPhCoverageByDefault(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must not mention PhCoverage when coverageFile is not set",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(this.program)
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(this.compiled)
            ).asString(),
            Matchers.not(Matchers.containsString("PhCoverage"))
        );
    }

    @Test
    void throwsDetailedError(@Mktmp final Path temp) {
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram("# Absent.")
                .execute(new FakeMaven.Transpile()),
            "TranspileMojo should throw an exception on invalid EO code"
        );
        final StringWriter writer = new StringWriter();
        exception.printStackTrace(new PrintWriter(writer));
        MatcherAssert.assertThat(
            "TranspileMojo should throw an exception with detailed message on invalid EO code",
            writer.toString(),
            Matchers.allOf(
                Matchers.containsString("Expected 1 child nodes, but found 0"),
                Matchers.containsString("main.xmir' encountered some problems, broken syntax?")
            )
        );
    }

    @Test
    void doesNotTouchAtom(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "TranspileMojo should not touch atoms, but it did",
            new FakeMaven(temp).withProgram(
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.0.0",
                "+unlint not-empty-atom",
                String.format("+version 0.0.0%n"),
                "[x y z] > main /bytes"
                )
                .execute(new FakeMaven.Transpile())
                .result(),
            Matchers.not(
                Matchers.allOf(
                    Matchers.hasKey(String.format("target/%s/foo/x/main.xmir", Transpiling.DIR)),
                    Matchers.hasKey("target/generated/EO_com/EO_example/EOfoo.java")
                )
            )
        );
    }

    @Test
    void createsPackageInfoFilesForAllPackages(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "TranspileMojo must generate package-info.java files for all of the packages",
            new FakeMaven(temp).withProgram(
                String.join(
                    System.lineSeparator(),
                    "+custom-meta",
                    "+package foo.x",
                    "",
                    "[] > main"
                )
                )
                .execute(new FakeMaven.Transpile())
                .result(),
            Matchers.allOf(
                Matchers.hasKey("target/generated/org/eolang/EO_foo/package-info.java"),
                Matchers.hasKey("target/generated/org/eolang/EO_foo/EO_x/package-info.java")
            )
        );
    }

    @Test
    void savesValidContentToPackageInfoFile(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "TranspileMojo must save valid content to package-info.java file",
            new TextOf(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "+package foo.x",
                        "",
                        "[] > main",
                        "  true > @"
                    )
                    )
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get("target/generated/org/eolang/EO_foo/EO_x/package-info.java")
            ).asString(),
            Matchers.allOf(
                Matchers.containsString("// @org.eolang.XmirPackage(\"foo.x\")"),
                Matchers.containsString("package org.eolang.EO_foo.EO_x;")
            )
        );
    }

    @Test
    void omitsPhSafeWrappersByDefault(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "TranspileMojo must skip PhSafe wrappers by default, but it did not",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(String.format("+package foo.x%n%n[] > main%n  42.plus 1 > @"))
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(this.compiled)
            ).asString(),
            Matchers.not(Matchers.containsString("new PhSafe("))
        );
    }

    @Test
    void wrapsDispatchedObjectsWithPhSafeWhenEnabled(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "TranspileMojo must wrap dispatched objects with PhSafe when enabled, but it did not",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(String.format("+package foo.x%n%n[] > main%n  42.plus 1 > @"))
                    .with("trackLocations", true)
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(this.compiled)
            ).asString(),
            Matchers.containsString("new PhSafe(")
        );
    }

    @Test
    void recompilesIfModified(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(this.compiled);
        final long before = java.toFile().lastModified();
        MatcherAssert.assertThat(
            "The timestamp of file should be updated",
            res.get("foo/x/main.eo").toFile().setLastModified(before + 1L),
            Matchers.is(true)
        );
        maven.execute(new FakeMaven.Transpile());
        MatcherAssert.assertThat(
            "The Java file should be recompiled",
            java.toFile().lastModified(),
            Matchers.greaterThan(before)
        );
    }

    @Test
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    void recompilesIfExpired(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(this.compiled);
        final Path xmir = maven.targetPath().resolve(
            String.format("%s/foo/x/main.xmir", Transpiling.DIR)
        );
        MatcherAssert.assertThat(
            "The Java file should exist after transpile",
            java.toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            "The Xmir file should exist after transpile",
            xmir.toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            "The Java file's last modified timestamp should be successfully reset",
            java.toFile().setLastModified(0L),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "The Xmir file's last modified timestamp should be successfully reset",
            xmir.toFile().setLastModified(0L),
            Matchers.is(true)
        );
        final long before = java.toFile().lastModified();
        maven.execute(MjTranspile.class);
        final long after = java.toFile().lastModified();
        MatcherAssert.assertThat(
            "The Java file should have a valid last modified timestamp after recompilation",
            after,
            Matchers.greaterThan(0L)
        );
        MatcherAssert.assertThat(
            "The Java file's last modified timestamp should change after recompilation",
            before,
            Matchers.not(Matchers.equalTo(after))
        );
    }

    @Test
    void doesNotRetranspileIfNotModified(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Path java = maven
            .withProgram(this.program)
            .allTojosWithHash(CommitHash.FAKE)
            .execute(new FakeMaven.Transpile())
            .result()
            .get(this.compiled);
        MatcherAssert.assertThat(
            "The .java file must be generated after first transpilation",
            java.toFile(),
            FileMatchers.anExistingFile()
        );
        MatcherAssert.assertThat(
            "The last modified date of generated .java file must be successfully set",
            java.toFile().setLastModified(0L),
            Matchers.is(true)
        );
        maven.execute(MjTranspile.class);
        MatcherAssert.assertThat(
            "The .java file must not be regenerated after repeat transpilation",
            java.toFile().lastModified(),
            Matchers.is(0L)
        );
    }

    @Test
    void transpilesSimpleEoProgram(@Mktmp final Path temp) throws Exception {
        final Map<String, Path> res = new FakeMaven(temp)
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        MatcherAssert.assertThat(
            "transpiled class must be present",
            res, Matchers.hasKey(this.compiled)
        );
        MatcherAssert.assertThat(
            "transpiled class must contain EOmain",
            new TextOf(res.get(this.compiled)).asString(),
            Matchers.containsString("class EOmain")
        );
    }

    @Test
    void transpilesSeveralEoProgramsInParallel(@Mktmp final Path temp) throws Exception {
        final int total = 30;
        final FakeMaven maven = new FakeMaven(temp);
        for (int prog = 1; prog < total; ++prog) {
            final String main = String.format("main%s", FakeMaven.suffix(prog));
            maven.withProgram(
                this.program.replace("main", main),
                String.format("foo.x.%s", main),
                String.format("foo/x/%s.eo", main)
            );
        }
        final List<Path> files;
        try (
            Stream<Path> list = Files.list(
                maven
                    .execute(new FakeMaven.Transpile())
                    .generatedPath()
                    .resolve("org/eolang/EO_foo/EO_x")
            )
        ) {
            files = list.collect(Collectors.toList());
        }
        MatcherAssert.assertThat(
            "All programs must be transpiled",
            files.size(),
            Matchers.equalTo(total)
        );
        for (final Path file : files) {
            final String java = new TextOf(file).asString();
            if (file.getFileName().toString().startsWith("EOmain")) {
                MatcherAssert.assertThat(
                    String.format(
                        "Generated %s must contain a class declaration, not be truncated/garbled by a concurrent transpilation race",
                        file
                    ),
                    java,
                    Matchers.containsString("class EOmain")
                );
            }
            MatcherAssert.assertThat(
                String.format(
                    "Generated %s must have balanced braces, not be truncated/garbled by a concurrent transpilation race",
                    file
                ),
                MjTranspileTest.balanced(java, '{', '}'),
                Matchers.equalTo(true)
            );
            MatcherAssert.assertThat(
                String.format(
                    "Generated %s must have balanced parentheses, not be truncated/garbled by a concurrent transpilation race",
                    file
                ),
                MjTranspileTest.balanced(java, '(', ')'),
                Matchers.equalTo(true)
            );
        }
    }

    @Test
    void transpilesSourcesForDifferentScopesWithoutIntersections(
        @Mktmp final Path temp
    ) throws IOException {
        final Path target = temp.resolve("target");
        final Path sources = target.resolve("generated-sources");
        final Path tests = target.resolve("generated-test-sources");
        final FakeMaven maven = new FakeMaven(temp);
        maven
            .with("generatedDir", sources.toFile())
            .with("targetDir", target.resolve("eo-sources").toFile())
            .withHelloWorld()
            .execute(new FakeMaven.Transpile());
        maven
            .with("scope", "test")
            .with("generatedDir", tests.toFile())
            .with("targetDir", target.resolve("eo-test-sources").toFile()).withProgram(
                this.program.replace("main", "main-1")
            )
            .execute(new FakeMaven.Transpile());
        MatcherAssert.assertThat(
            "TranspileMojo should have processed exactly 2 files",
            maven.foreign().size(),
            Matchers.equalTo(2)
        );
        final Set<String> intersection = MjTranspileTest.classes(tests);
        intersection.retainAll(MjTranspileTest.classes(sources));
        MatcherAssert.assertThat(
            "Both class paths should not intersect and don't have to have common classes",
            intersection,
            Matchers.allOf(
                Matchers.iterableWithSize(1),
                Matchers.hasItem("package-info.java")
            )
        );
    }

    /**
     * Get all classes in directory.
     * @param root Directory to get classes from
     * @return Set of classes
     * @throws IOException If fails.
     */
    @SuppressWarnings("PMD.UnnecessaryLocalRule")
    private static Set<String> classes(final Path root) throws IOException {
        try (Stream<Path> walk = Files.walk(root)) {
            return walk.filter(MjTranspileTest::isJava)
                .map(MjTranspileTest::filename)
                .collect(Collectors.toSet());
        }
    }

    /**
     * Is java file.
     * @param path Path to check
     * @return True if path is java file
     */
    private static boolean isJava(final Path path) {
        return Files.isRegularFile(path) && path.toString().endsWith(".java");
    }

    /**
     * Get filename.
     * @param path Path to get filename from
     * @return Filename
     */
    private static String filename(final Path path) {
        return path.getFileName().toString();
    }

    /**
     * Check that every opening bracket has a matching closing one, in order.
     * @param text Text to check
     * @param open Opening bracket character
     * @param close Closing bracket character
     * @return TRUE if brackets are balanced
     */
    private static boolean balanced(final String text, final char open, final char close) {
        int depth = 0;
        boolean valid = true;
        for (int idx = 0; idx < text.length(); ++idx) {
            final char chr = text.charAt(idx);
            if (chr == open) {
                depth = depth + 1;
            } else if (chr == close) {
                depth = depth - 1;
            }
            if (depth < 0) {
                valid = false;
                break;
            }
        }
        return valid && depth == 0;
    }
}
