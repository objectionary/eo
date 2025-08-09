/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.TrDefault;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link MjTranspile}.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjTranspileTest {

    /**
     * Test eo program from resources.
     */
    private String program;

    /**
     * Traspiled to java eo program from resources.
     */
    private String compiled;

    @BeforeEach
    void setUp() throws Exception {
        this.program = new TextOf(new ResourceOf("org/eolang/maven/mess.eo")).asString();
        this.compiled = "target/generated/EOfoo/EOx/EOmainTest.java";
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/transpile-packs/", glob = "**.yaml")
    void checksTranspilePacks(final String yaml) {
        MatcherAssert.assertThat(
            "passed without exceptions",
            new XtSticky(
                new XtYaml(
                    yaml,
                    eo -> new EoSyntax(
                        new InputOf(String.format("%s\n", eo))
                    ).parsed(),
                    new TrDefault<>()
                )
            ),
            new XtoryMatcher()
        );
    }

    @Test
    void doesNotTouchAtom(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package foo.x",
                "+rt jvm org.eolang:eo-runtime:0.0.0\n",
                "# Atom.",
                "[x y z] > main ?"
            );
        final Map<String, Path> res = maven
            .execute(new FakeMaven.Transpile())
            .result();
        MatcherAssert.assertThat(
            "TranspileMojo should not touch atoms, but it did",
            res,
            Matchers.not(
                Matchers.allOf(
                    Matchers.hasKey(String.format("target/%s/foo/x/main.xmir", MjTranspile.DIR)),
                    Matchers.hasKey("target/generated/EOcom/EOexample/EOfoo.java")
                )
            )
        );
    }

    @Test
    void createsPackageInfoFilesForAllPackages(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "TranspileMojo must generate package-info.java files for all of the packages",
            new FakeMaven(temp)
                .withProgram(
                    "+custom-meta",
                    "+package foo.x\n",
                    "# Simple.",
                    "[] > main"
                )
                .execute(new FakeMaven.Transpile())
                .result(),
            Matchers.allOf(
                Matchers.hasKey("target/generated/EOfoo/package-info.java"),
                Matchers.hasKey("target/generated/EOfoo/EOx/package-info.java")
            )
        );
    }

    @Test
    void savesValidContentToPackageInfoFile(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "TranspileMojo must save valid content to package-info.java file",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(
                        "+package foo.x\n",
                        "# Simple.",
                        "[] > main",
                        "  true > @"
                    )
                    .execute(new FakeMaven.Transpile())
                    .result()
                    .get("target/generated/EOfoo/EOx/package-info.java")
            ).asString(),
            Matchers.allOf(
                Matchers.containsString("// @org.eolang.XmirPackage(\"foo.x\")"),
                Matchers.containsString("package EOfoo.EOx;")
            )
        );
    }

    @Disabled
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

    @Disabled
    @Test
    void recompilesIfExpired(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven
            .withProgram(this.program)
            .execute(new FakeMaven.Transpile())
            .result();
        final Path java = res.get(this.compiled);
        final Path xmir = maven.targetPath().resolve(
            String.format("%s/foo/x/main.xmir", MjTranspile.DIR)
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

    @Disabled
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

    @Disabled
    @Test
    void transpilesSimpleEoProgram(@Mktmp final Path temp) throws Exception {
        final Path src = Paths.get("../eo-runtime/src/main/eo/org/eolang/tuple.eo");
        final Map<String, Path> res = new FakeMaven(temp)
            .withProgram(
                new TextOf(src).asString(),
                "org.eolang.tuple",
                "org/eolang/tuple.eo"
            )
            .execute(new FakeMaven.Transpile())
            .result();
        final String java = "target/generated/EOorg/EOeolang/EOtuple.java";
        MatcherAssert.assertThat(
            "transpiled class must be present",
            res, Matchers.hasKey(java)
        );
        MatcherAssert.assertThat(
            "package-info.java files must be present",
            res,
            Matchers.hasKey("target/generated/EOorg/EOeolang/package-info.java")
        );
        MatcherAssert.assertThat(
            "transpiled class must contain EOtuple",
            new TextOf(res.get(java)).asString(),
            Matchers.containsString("class EOtuple")
        );
    }

    @Disabled
    @Test
    void transpilesSeveralEoProgramsInParallel(@Mktmp final Path temp) throws IOException {
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
        MatcherAssert.assertThat(
            "All programs must be transpiled",
            Files.list(
                maven
                    .execute(new FakeMaven.Transpile())
                    .generatedPath()
                    .resolve("EOfoo")
                    .resolve("EOx")
            ).count(),
            Matchers.equalTo((long) total)
        );
    }

    @Disabled
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
            .with("targetDir", target.resolve("eo-test-sources").toFile())
            .withProgram(
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
     * @param root Directory to get classes from.
     * @return Set of classes.
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
     * @param path Path to check.
     * @return True if path is java file.
     */
    private static boolean isJava(final Path path) {
        return Files.isRegularFile(path) && path.toString().endsWith(".java");
    }

    /**
     * Get filename.
     * @param path Path to get filename from.
     * @return Filename.
     */
    private static String filename(final Path path) {
        return path.getFileName().toString();
    }
}
