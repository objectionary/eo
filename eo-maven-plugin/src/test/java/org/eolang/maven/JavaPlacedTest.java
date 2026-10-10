/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.maven.project.MavenProject;
import org.cactoos.text.TextOf;
import org.eolang.cache.Saved;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Tests for {@link JavaPlaced}.
 *
 * @since 0.56.7
 */
@ExtendWith(MktmpResolver.class)
final class JavaPlacedTest {

    @ParameterizedTest
    @CsvSource({
        "target/generated, TestAtomEOmain",
        "target/deep/generated, TestAtomEOmain",
        "generated, TestAtomEOmain",
        "target/deep/generated, TestEOmain"
    })
    void respectsConfiguredRoots(
        final String output, final String name, @Mktmp final Path temp
    ) throws Exception {
        final MavenProject project = new MavenProject();
        final Path root = temp.resolve("target/handwritten");
        project.addTestCompileSourceRoot(root.toString());
        if (name.contains("Atom")) {
            new Saved(
                "package org.eolang.EO_foo.EO_x; final class TestEOmain {}",
                root.resolve("org/eolang/EO_foo/EO_x/TestEOmain.java")
            ).value();
        }
        final Path generated = temp.resolve(output);
        project.addTestCompileSourceRoot(
            generated.getParent().resolve("generated-test-sources/.").toString()
        );
        final FakeMaven maven = new FakeMaven(temp).withProgram(
            String.format(
                "+architect %s%n+package foo.x%n%n[] > main%n%n  ++> can-work%n    true > @",
                "yegor256@gmail.com"
            )
        ).with("project", project).with("generated", generated.toFile());
        maven.execute(new PpTranspile()).execute(MjTranspile.class);
        MatcherAssert.assertThat(
            "Configured handwritten tests must rename companions without counting generated tests",
            new TextOf(
                generated.getParent().resolve("generated-test-sources/org/eolang/EO_foo/EO_x")
                    .resolve(String.format("%s.java", name))
            ).asString(),
            Matchers.containsString(String.format("class %s ", name))
        );
    }

    @Test
    void placesJavaGeneratedCode(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("target").resolve("Foo.java");
        final String expected = "public final class Main {}";
        final Path generated = temp.resolve("generated-sources");
        final Xnav java = new Xnav(
            new Xembler(
                new Directives().add("class").attr("java-name", "Foo").add("java").set(expected)
            ).xml()
        ).element("class");
        new JavaPlaced(
            new FpJavaGenerated(
                java,
                new FileGenerationReport(new AtomicInteger(), generated, target)
            ),
            target,
            generated
        ).exec(java, false);
        MatcherAssert.assertThat(
            "Generated Java code does not match with expected",
            new TextOf(target).asString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void placesJavaChecks(@Mktmp final Path temp) throws Exception {
        final String expected = String.join(
            System.lineSeparator(),
            "final class FooTest {",
            "  @Test",
            "  void testsSomething() {}",
            "}"
        );
        final Path target = temp.resolve("target");
        final Path generated = target.resolve("generated-sources");
        final Path utest = target.resolve("FooTest.java");
        final Xnav java = new Xnav(
            new Xembler(
                new Directives().add("class").attr("java-name", "Foo").add("tests").set(expected)
            ).xml()
        ).element("class");
        new JavaPlaced(
            new FpJavaGenerated(java, generated, utest), utest, generated
        ).exec(java, true);
        MatcherAssert.assertThat(
            "Generated tests does not match with expected",
            new TextOf(
                target.resolve("generated-test-sources").resolve("TestFoo.java")
            ).asString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void placesClassMarkedOnlyWithParameterized(@Mktmp final Path temp) throws Exception {
        final String expected = String.join(
            System.lineSeparator(),
            "final class FooTest {",
            "  @ParameterizedTest",
            "  @ValueSource(ints = {1, 2})",
            "  void testsSomething(final int arg) {}",
            "}"
        );
        final Path target = temp.resolve("target");
        final Path generated = target.resolve("generated-sources");
        final Path utest = target.resolve("FooTest.java");
        final Xnav java = new Xnav(
            new Xembler(
                new Directives().add("class").attr("java-name", "Foo").add("tests").set(expected)
            ).xml()
        ).element("class");
        new JavaPlaced(
            new FpJavaGenerated(java, generated, utest), utest, generated
        ).exec(java, true);
        MatcherAssert.assertThat(
            "A generated class marked only with @ParameterizedTest was silently skipped",
            new TextOf(
                target.resolve("generated-test-sources").resolve("TestFoo.java")
            ).asString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void removesObsoleteJavaCompanions(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("target");
        final Path generated = target.resolve("generated-sources");
        final Path utest = target.resolve("FooTest.java");
        final JavaPlaced placed = new JavaPlaced(
            new FpJavaGenerated(this.clazz("@Test"), generated, utest), utest, generated
        );
        placed.exec(this.clazz("@Test"), true);
        final Path test = target.resolve("generated-test-sources").resolve("TestFoo.java");
        final boolean created = Files.exists(test);
        placed.exec(this.clazz(""), true);
        MatcherAssert.assertThat(
            "Obsolete Java test was not removed", created && Files.notExists(test)
        );
    }

    @Test
    void removesCompanionsWhenNoneAreTranspiled(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("target");
        final Path generated = target.resolve("generated-sources");
        final Path utest = target.resolve("FooTest.java");
        final JavaPlaced placed = new JavaPlaced(
            new FpJavaGenerated(this.clazz("@Test"), generated, utest), utest, generated
        );
        placed.exec(this.clazz("@Test"), true);
        final Path test = target.resolve("generated-test-sources").resolve("TestFoo.java");
        final boolean created = Files.exists(test);
        placed.exec(this.clazz("@Test"), false);
        MatcherAssert.assertThat(
            "A test of a previous build survived a transpile that asked for no tests",
            created && Files.notExists(test)
        );
    }

    @Test
    void removesObsoleteAtomJavaCompanions(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("target");
        final Path generated = target.resolve("generated-sources");
        final Path utest = target.resolve("FooTest.java");
        final JavaPlaced placed = new JavaPlaced(
            new FpJavaGenerated(this.clazz("@Test"), generated, utest), utest, generated,
            temp.resolve("src/test/java")
        );
        Files.createDirectories(temp.resolve("src/test/java"));
        new Saved("", temp.resolve("src/test/java/TestFoo.java")).value();
        placed.exec(this.clazz("@Test"), true);
        final Path atom = target.resolve("generated-test-sources").resolve("TestAtomFoo.java");
        final boolean created = Files.exists(atom);
        placed.exec(this.clazz(""), true);
        MatcherAssert.assertThat(
            "Obsolete atom Java test was not removed", created && Files.notExists(atom)
        );
    }

    private Xnav clazz(final String tests) throws Exception {
        return new Xnav(
            new Xembler(
                new Directives().add("class").attr("java-name", "Foo").add("tests").set(tests)
            ).xml()
        ).element("class");
    }
}
