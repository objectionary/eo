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
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Tests for {@link JavaPlaced}.
 * @since 0.56.7
 */
@ExtendWith(MktmpResolver.class)
final class JavaPlacedTest {

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
            new FpJavaGenerated(this.clazz("@Test"), generated, utest), utest, generated
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
