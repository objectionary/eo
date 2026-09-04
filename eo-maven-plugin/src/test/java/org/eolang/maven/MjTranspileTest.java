/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
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
import org.eolang.xax.Xtory;
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
        final Xtory story = new XtSticky(
            new XtYaml(
                yaml,
                eo -> new EoSyntax(
                    new InputOf(String.format("%s%n", eo))
                ).parsed(),
                new TrDefault<>()
            )
        );
        Assumptions.assumeTrue(story.map().get("skip") == null);
        MatcherAssert.assertThat(
            "passed without exceptions",
            story,
            new XtoryMatcher()
        );
    }

    @Test
    void givesDistinctClassNamesToLongNamesDifferingBeyondTheLimit() throws IOException {
        final String head = String.join("", Collections.nCopies(249, "a"));
        MatcherAssert.assertThat(
            "two names that differ only past the length limit must not share a Java class name",
            this.javaName(String.format("%sAaaaazaaaaaaaaaaaaaaa", head)),
            Matchers.not(
                Matchers.equalTo(
                    this.javaName(String.format("%staaaaHaaaaaaaaaaaaaaa", head))
                )
            )
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
    void tracksStepsOfASourceTheCacheAlreadyHolds(@Mktmp final Path temp) throws IOException {
        final Path cache = temp.resolve("cache");
        final String src = MjTranspileTest.pair();
        new FakeMaven(temp.resolve("first"))
            .withProgram(src)
            .with("cache", cache.toFile())
            .with("trackSteps", true)
            .execute(MjParse.class)
            .execute(MjTranspile.class);
        MatcherAssert.assertThat(
            "a second build with the same flag and source must write the steps again, but the cache took them away",
            new FakeMaven(temp.resolve("second"))
                .withProgram(src)
                .with("cache", cache.toFile())
                .with("trackSteps", true)
                .execute(MjParse.class)
                .execute(MjTranspile.class)
                .result(),
            Matchers.hasKey(
                String.format("target/%s/examples/x/01-set-locators.xml", Transpiling.PRE)
            )
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
                String.format("target/%s/examples/x/01-set-locators.xml", Transpiling.PRE)
            )
        );
    }

    @Test
    void marksSafeToCacheFormationOfTranspiledProgram(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a formation that takes nothing and copies nothing but a literal must be marked as safe to cache, but it wasnt",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "+package examples",
                        "",
                        "# Outer.",
                        "[] > x",
                        "  inner > @",
                        "  # Inner.",
                        "  [] > inner",
                        "    42 > @"
                    )
                )
                .with("trackSteps", true)
                .execute(MjParse.class)
                .execute(MjInference.class)
                .execute(MjTranspile.class)
                .targetPath()
                .resolve(Transpiling.PRE)
                .resolve("examples")
                .resolve("x")
                .resolve("10-purify.xml")
            ),
            XhtmlMatchers.hasXPath("//abstract[@name='inner' and @pure='true']")
        );
    }

    @Test
    void wrapsSafeToCacheFormationInPhSticky(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a formation marked as safe to cache must be wrapped in PhSticky in the generated Java, but it wasnt",
            new TextOf(
                MjTranspileTest.pure(temp)
                    .execute(MjParse.class)
                    .execute(MjInference.class)
                    .execute(MjTranspile.class)
                    .result()
                    .get("target/generated/org/eolang/EO_examples/EOx.java")
            ).asString(),
            Matchers.containsString("new PhSticky(")
        );
    }

    @Test
    void marksTopLevelPureFormationWithPureMarker(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a top-level formation marked as safe to cache must implement Pure in the generated Java, so PhPackage can wrap it in PhSticky, but it didnt",
            new TextOf(
                MjTranspileTest.pure(temp)
                    .execute(MjParse.class)
                    .execute(MjInference.class)
                    .execute(MjTranspile.class)
                    .result()
                    .get("target/generated/org/eolang/EO_examples/EOx.java")
            ).asString(),
            Matchers.containsString("implements Pure")
        );
    }

    @Test
    void wrapsAnonymousPureFormationInPhSticky(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "an anonymous formation marked as safe to cache must be wrapped in PhSticky in the generated Java, but it wasnt",
            new TextOf(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "+package examples",
                        "",
                        "# Outer.",
                        "[] > x",
                        "  seq > @",
                        "    []",
                        "      42 > @"
                    )
                )
                .execute(MjParse.class)
                .execute(MjInference.class)
                .execute(MjTranspile.class)
                .result()
                .get("target/generated/org/eolang/EO_examples/EOx.java")
            ).asString(),
            Matchers.containsString("new PhSticky(")
        );
    }

    @Test
    void wrapsApplicationOfDataInPhSticky(@Mktmp final Path temp) throws IOException {
        final Path parsed = Files.createDirectories(temp.resolve("parsed"));
        Files.writeString(
            parsed.resolve("app.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > app", "  2.plus 3 > x", "  x > @", ""
                )
            ).parsed().toString()
        );
        Files.writeString(
            parsed.resolve("number.xmir"),
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[as-bytes] > number", "  as-bytes > @",
                    "  [x] > plus", "    x > @", ""
                )
            ).parsed().toString()
        );
        final Path tables = temp.resolve("tables");
        new Inferring(parsed, temp.resolve("pre"), tables).exec();
        MatcherAssert.assertThat(
            "an application whose parts are all data must be wrapped in PhSticky, but it wasnt",
            new Xsline(
                new TrDefault<Shift>()
                    .with(new StClasspath("/org/eolang/parser/parse/set-locators.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/set-original-names.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/classes.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/attrs.xsl"))
                    .with(new StClasspath("/org/eolang/maven/transpile/data.xsl"))
                    .with(new StPure("/org/eolang/maven/transpile/purify.xsl", tables))
                    .with(new StClasspath("/org/eolang/maven/transpile/to-java.xsl"))
            ).pass(new XMLDocument(parsed.resolve("app.xmir"))).toString(),
            Matchers.containsString("new PhSticky(new PhApplication(")
        );
    }

    @Test
    void leavesUnmarkedFormationBare(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "a formation nobody marked as safe to cache must not be wrapped in PhSticky, but it was",
            new TextOf(
                MjTranspileTest.pure(temp)
                    .execute(MjParse.class)
                    .execute(MjTranspile.class)
                    .result()
                    .get("target/generated/org/eolang/EO_examples/EOx.java")
            ).asString(),
            Matchers.not(Matchers.containsString("new PhSticky("))
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
                    .with("coverage", true)
                    .execute(new PpTranspile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.containsString("new PhCoverage(")
        );
    }

    @Test
    void excludesThrowingCasesFromPhCoverageWhenTrackingEnabled(@Mktmp final Path temp)
        throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must not wrap a throwing test's body into PhCoverage when coverageTracking is on",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.throwing())
                    .with("coverage", true)
                    .execute(new PpTranspile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.not(Matchers.containsString("new PhCoverage("))
        );
    }

    @Test
    void excludesTruthyCasesFromPhCoverageWhenTrackingEnabled(@Mktmp final Path temp)
        throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must not wrap a truthy test's body into PhCoverage when coverageTracking is on",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.truthy())
                    .with("coverage", true)
                    .execute(new PpTranspile())
                    .result()
                    .get(MjTranspileTest.compiled())
            ).asString(),
            Matchers.not(Matchers.containsString("new PhCoverage("))
        );
    }

    @Test
    void keepsGeneratedJavaFreeOfPhCoverageByDefault(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the generated Java must not mention PhCoverage when coverageTracking is off",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(MjTranspileTest.program())
                    .execute(new PpTranspile())
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
                    .execute(new PpTranspile())
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
                    .execute(new PpTranspile())
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
                    .execute(new PpTranspile())
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
            .execute(new PpTranspile());
        MatcherAssert.assertThat(
            "the second run's generated Java must reflect its own phiDefaultClass instead of reusing the first run's cached PhDefault output",
            new TextOf(
                new FakeMaven(temp.resolve("second"))
                    .withProgram(src)
                    .with("cache", cache.toFile())
                    .with("superclass", "org.example.PhInspected")
                    .execute(new PpTranspile())
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
                .execute(new PpTranspile()),
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
                .execute(new PpTranspile()),
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
                .execute(new PpTranspile())
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
    void namesTheAtomOfAPackageMemberInsideItsPackageObject(@Mktmp final Path temp)
        throws Exception {
        MatcherAssert.assertThat(
            "the atom of a package member must be a class nested in the class of the package object, which is where the library that ships it puts it (#8295)",
            new TextOf(
                MjTranspileTest.withMember(temp)
                    .execute(MjParse.class)
                    .execute(MjTranspile.class)
                    .result()
                    .get("target/generated/org/eolang/EOfoo.java")
            ).asString(),
            Matchers.containsString("new EOfoo$EObar$EObaz()")
        );
    }

    @Test
    void compilesAPackageMemberAsAPartOfItsObject(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a member of a package this build compiles an object for must not be compiled apart, even when the merge goal was never named",
            MjTranspileTest.withMember(temp)
                .execute(MjParse.class)
                .execute(MjTranspile.class)
                .result(),
            Matchers.not(Matchers.hasKey("target/generated/org/eolang/EO_foo/EObar.java"))
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
                    "+custommeta",
                    "+package foo.x",
                    "",
                    "[] > main"
                )
            )
            .execute(new PpTranspile())
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
                .execute(new PpTranspile())
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
                    .execute(new PpTranspile())
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
                    .with("located", true)
                    .execute(new PpTranspile())
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
            .execute(new PpTranspile());
        MatcherAssert.assertThat(
            "the second run's generated Java must reflect its own trackLocations=true setting instead of reusing the first run's cached trackLocations=false output",
            new TextOf(
                new FakeMaven(temp.resolve("second"))
                    .withProgram(src)
                    .with("cache", cache.toFile())
                    .with("located", true)
                    .execute(new PpTranspile())
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
            .execute(new PpTranspile())
            .result();
        final Path java = res.get(MjTranspileTest.compiled());
        final long before = java.toFile().lastModified();
        Assumptions.assumeTrue(
            res.get("foo/x/main.eo").toFile().setLastModified(before + 1L),
            "The filesystem refused to touch the source, cannot tell modified from intact"
        );
        maven.execute(new PpTranspile());
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
            .execute(new PpTranspile())
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
            .execute(new PpTranspile())
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
                    .execute(new PpTranspile())
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
                    .execute(new PpTranspile())
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
            .execute(new PpTranspile());
        maven
            .with("scope", "test")
            .with("generatedDir", tests.toFile())
            .with("targetDir", target.resolve("eo-test-sources").toFile()).withProgram(
                MjTranspileTest.program().replace("main", "main-1")
            )
            .execute(new PpTranspile());
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

    private static String instantiating() {
        return String.format(
            "+architect yegor256@gmail.com%n+package foo.x%n%n[] > main%n  [] > inner%n    42 > @%n  42.plus > @%n    []"
        );
    }

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

    private static String throwing() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "",
            "  --> stops-on-dispatching-on-a-number",
            "    42.plus 1 > @"
        );
    }

    private static String truthy() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "",
            "  ++> can-dispatch-on-a-number",
            "    42.plus 1 > @"
        );
    }

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

    private static String program() {
        return new UncheckedText(
            new TextOf(new ResourceOf("org/eolang/maven/mess.eo"))
        ).asString();
    }

    private static String compiled() {
        return "target/generated/org/eolang/EO_foo/EO_x/EOmain.java";
    }

    private static Set<String> classes(final Path root) throws IOException {
        try (Stream<Path> walk = Files.walk(root)) {
            return walk.filter(MjTranspileTest::isJava)
                .map(MjTranspileTest::filename)
                .collect(Collectors.toSet());
        }
    }

    private static boolean isJava(final Path path) {
        return Files.isRegularFile(path) && path.toString().endsWith(".java");
    }

    private static String filename(final Path path) {
        return path.getFileName().toString();
    }

    private static boolean intact(final Path file) {
        final String java = new UncheckedText(new TextOf(file)).asString();
        return MjTranspileTest.balanced(java, '{', '}')
            && MjTranspileTest.balanced(java, '(', ')')
            && (!MjTranspileTest.filename(file).startsWith("EOmain")
                || java.contains("class EOmain"));
    }

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

    private static FakeMaven pure(final Path temp) throws IOException {
        return new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "+package examples",
                "",
                "# Outer.",
                "[] > x",
                "  inner > @",
                "  # Inner.",
                "  [] > inner",
                "    42 > @"
            )
        );
    }

    private String javaName(final String name) throws IOException {
        return new Xsline(
            new TrClasspath<>(
                "/org/eolang/parser/parse/set-locators.xsl",
                "/org/eolang/maven/transpile/set-original-names.xsl",
                "/org/eolang/maven/transpile/classes.xsl",
                "/org/eolang/maven/transpile/attrs.xsl",
                "/org/eolang/maven/transpile/data.xsl",
                "/org/eolang/maven/transpile/to-java.xsl"
            ).back()
        ).pass(
            new EoSyntax(String.format("[] > %s%n  42 > @%n", name)).parsed()
        ).xpath("//@java-name").get(0);
    }

    // A workspace with an object and a member of its package, where the
    // member holds an atom.
    private static FakeMaven withMember(final Path temp) throws IOException {
        return new FakeMaven(temp).withProgram(
            String.join(
                System.lineSeparator(),
                "[] > foo",
                "  42 > @"
            ),
            "foo",
            "foo.eo"
        ).withProgram(
            String.join(
                System.lineSeparator(),
                "+package foo",
                "+rt jvm org.eolang:eo-runtime:0.0.0",
                "",
                "[] > bar",
                "  [] > baz /bytes",
                "    ? > x"
            ),
            "foo.bar",
            "foo/bar.eo"
        );
    }
}
