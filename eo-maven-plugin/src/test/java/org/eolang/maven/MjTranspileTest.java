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
import org.cactoos.text.UncheckedText;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link MjTranspile}.
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjTranspileTest {

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
                ).with("trackSteps", true)
                .execute(MjParse.class)
                .execute(MjTranspile.class),
            "We should be able to transpile a simple EO program without exceptions when tracking transformation steps"
        );
    }

    @Test
    void tracksStepsOfProgramWithTwoObjects(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the first tracked step of a program holding two objects did not leave its XMIR in the pre-transpile directory",
            new FakeMaven(temp).withProgram(MjTranspileTest.pair())
                .with("trackSteps", true)
                .execute(MjParse.class)
                .execute(MjTranspile.class)
                .result(),
            Matchers.hasKey(
                String.format("target/%s/examples/x/00-set-locators.xml", Transpiling.PRE)
            )
        );
    }

    @Test
    void transpilesSecondObjectOfProgramWhileTrackingSteps(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "the second object of a tracked program did not reach the generated Java",
            new FakeMaven(temp).withProgram(MjTranspileTest.pair())
                .with("trackSteps", true)
                .execute(MjParse.class)
                .execute(MjTranspile.class)
                .result(),
            Matchers.hasKey("target/generated/org/eolang/EO_examples/EOy.java")
        );
    }

    @Test
    void wrapsObjectsIntoPhCoverageWhenTrackingEnabled(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must wrap located objects into PhCoverage when coverageTracking is on",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.program())
                    .with("coverageTracking", true)
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.containsString("new PhCoverage(")
        );
    }

    @Test
    void keepsGeneratedJavaFreeOfPhCoverageByDefault(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must not mention PhCoverage when coverageTracking is off",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.program())
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.not(Matchers.containsString("PhCoverage"))
        );
    }

    @Test
    void extendsPhDefaultInGeneratedJavaByDefault(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated class must extend PhDefault when phiDefaultClass is not set",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.plain())
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.containsString("extends PhDefault {")
        );
    }

    @Test
    void extendsGivenPhiClassInGeneratedJava(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated class must extend the class named by phiDefaultClass instead of PhDefault",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.plain())
                    .with("superclass", "org.example.PhInspected")
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.containsString("extends org.example.PhInspected {")
        );
    }

    @Test
    void buildsGivenPhiClassAtInstantiationSites(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must instantiate the class named by phiDefaultClass, never PhDefault, so that the whole tree is made of the substituted class",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.instantiating())
                    .with("superclass", "org.example.PhInspected")
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.not(Matchers.containsString("new PhDefault"))
        );
    }

    @Test
    void invalidatesCacheWhenPhiDefaultClassChanges(@Mktmp final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final String src = MjTranspileTest.plain();
        new FakeMaven(temp.resolve("first"))
            .withProgram(src)
            .with("cache", cache.toFile())
            .execute(new FakeMaven.Transpile());
        MatcherAssert.assertThat(
            "the second run's generated Java must reflect its own phiDefaultClass instead of reusing the first run's cached PhDefault output",
            new TextOf(
                new FakeMaven(temp.resolve("second"))
                    .withProgram(src)
                    .with("cache", cache.toFile())
                    .with("superclass", "org.example.PhInspected")
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.containsString("extends org.example.PhInspected {")
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "org.example.Ph Inspected", "42Nope"})
    void rejectsPhiDefaultClassThatIsNotAJavaName(final String name, @Mktmp final Path temp) {
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(MjTranspileTest.program())
                .with("superclass", name)
                .execute(new FakeMaven.Transpile()),
            "a phiDefaultClass that is not a Java class name must not reach the generated Java"
        );
        final StringWriter writer = new StringWriter();
        exception.printStackTrace(new PrintWriter(writer));
        MatcherAssert.assertThat(
            "a phiDefaultClass that is not a Java class name must be refused by naming the option, instead of emitting an extends clause that cannot compile",
            writer.toString(),
            Matchers.containsString("eo.phiDefaultClass")
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
                "+architect yegor256@gmail.com",
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.0.0",
                "+unlint not-empty-atom",
                String.format("+version 0.0.0%n"),
                "[] > main /bytes",
                "  ? > x",
                "  ? > y",
                "  ? > z"
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
                    "+architect yegor256@gmail.com",
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
                        "+architect yegor256@gmail.com",
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
                Matchers.containsString("@org.eolang.XmirPackage(\"foo.x\")"),
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
                    .withProgram(MjTranspileTest.dispatching())
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
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
                    .withProgram(MjTranspileTest.dispatching())
                    .with("trackLocations", true)
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.containsString("new PhSafe(")
        );
    }

    @Test
    void invalidatesCacheWhenTrackLocationsChanges(@Mktmp final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final String src = MjTranspileTest.dispatching();
        new FakeMaven(temp.resolve("first"))
            .withProgram(src)
            .with("cache", cache.toFile())
            .execute(new FakeMaven.Transpile());
        MatcherAssert.assertThat(
            "the second run's generated Java must reflect its own trackLocations=true setting instead of reusing the first run's cached trackLocations=false output",
            new TextOf(
                new FakeMaven(temp.resolve("second"))
                    .withProgram(src)
                    .with("cache", cache.toFile())
                    .with("trackLocations", true)
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.containsString("new PhSafe(")
        );
    }

    @Test
    void recompilesIfModified(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(MjTranspileTest.program())
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(MjTranspileTest.compiled());
        final long before = java.toFile().lastModified();
        Assumptions.assumeTrue(
            res.get("foo/x/main.eo").toFile().setLastModified(before + 1L),
            "The filesystem refused to touch the source, cannot tell modified from intact"
        );
        maven.execute(new FakeMaven.Transpile());
        MatcherAssert.assertThat(
            "The Java file should be recompiled",
            java.toFile().lastModified(),
            Matchers.greaterThan(before)
        );
    }

    @Test
    void recompilesIfExpired(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(MjTranspileTest.program())
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(MjTranspileTest.compiled());
        Assumptions.assumeTrue(
            java.toFile().setLastModified(0L)
                && maven.targetPath()
                    .resolve(String.format("%s/foo/x/main.xmir", Transpiling.DIR))
                    .toFile()
                    .setLastModified(0L),
            "The filesystem refused to expire the transpiled files, cannot tell expired from fresh"
        );
        maven.execute(MjTranspile.class);
        MatcherAssert.assertThat(
            "The Java file should be recompiled once it and its XMIR expired",
            java.toFile().lastModified(),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    void doesNotRetranspileIfNotModified(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Path java = maven
            .withProgram(MjTranspileTest.program())
            .allTojosWithHash(CommitHash.FAKE)
            .execute(new FakeMaven.Transpile())
            .result()
            .get(MjTranspileTest.compiled());
        Assumptions.assumeTrue(
            java.toFile().setLastModified(0L),
            "The filesystem refused to expire the generated file, cannot tell reused from regenerated"
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
        MatcherAssert.assertThat(
            String.format(
                "Transpiled %s must contain EOmain, but it didnt", MjTranspileTest.compiled()
            ),
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.program())
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
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
                MjTranspileTest.program().replace("main", main),
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
            String.format(
                "Every one of %s must be transpiled into an intact Java source, but a concurrent transpilation race truncated or garbled some of them",
                files
            ),
            files.stream().filter(MjTranspileTest::intact).collect(Collectors.toList()),
            Matchers.hasSize(total)
        );
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
                MjTranspileTest.program().replace("main", "main-1")
            )
            .execute(new FakeMaven.Transpile());
        final Set<String> intersection = MjTranspileTest.classes(tests);
        intersection.retainAll(MjTranspileTest.classes(sources));
        MatcherAssert.assertThat(
            String.format(
                "The main and test scopes should share nothing but package-info.java, but they didnt, out of %d transpiled program(s)",
                maven.foreign().size()
            ),
            intersection,
            Matchers.allOf(
                Matchers.iterableWithSize(1),
                Matchers.hasItem("package-info.java")
            )
        );
    }

    /**
     * An EO program that makes the transpiler build objects at each of the
     * three places it emits a {@code new} of the base class: the context of
     * a generated {@code apply()} for the nested formation, the argument of
     * a {@code PhApplication} for the number, and an anonymous abstract
     * object with no children for the empty argument.
     * @return Source code of the program
     */
    private static String instantiating() {
        return String.format(
            "+architect yegor256@gmail.com%n+package foo.x%n%n[] > main%n  [] > inner%n    42 > @%n  42.plus > @%n    []"
        );
    }

    /**
     * The smallest EO program, with a single data attribute.
     * @return Source code of the program
     */
    private static String plain() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "  42 > @"
        );
    }

    /**
     * An EO program whose only attribute is a dispatch on a number.
     * @return Source code of the program
     */
    private static String dispatching() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "  42.plus 1 > @"
        );
    }

    /**
     * The EO program holding two top-level objects.
     * @return Source code of the program
     */
    private static String pair() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package examples",
            "",
            "# First.",
            "[] > x",
            "",
            "# Second.",
            "[] > y"
        );
    }

    /**
     * The EO program to transpile, taken from test resources.
     * @return Source code of the program
     */
    private static String program() {
        return new UncheckedText(
            new TextOf(new ResourceOf("org/eolang/maven/mess.eo"))
        ).asString();
    }

    /**
     * The Java file the program is transpiled into.
     * @return Path relative to the workspace
     */
    private static String compiled() {
        return "target/generated/org/eolang/EO_foo/EO_x/EOmain.java";
    }

    /**
     * Get all classes in directory.
     * @param root Directory to get classes from
     * @return Set of classes
     * @throws IOException If fails.
     */
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
     * Check that a generated Java source came out whole, with balanced
     * brackets and, for the main object, a class declaration.
     * @param file Generated Java file
     * @return TRUE if the source is intact
     */
    private static boolean intact(final Path file) {
        final String java = new UncheckedText(new TextOf(file)).asString();
        return MjTranspileTest.balanced(java, '{', '}')
            && MjTranspileTest.balanced(java, '(', ')')
            && (!MjTranspileTest.filename(file).startsWith("EOmain")
                || java.contains("class EOmain"));
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
