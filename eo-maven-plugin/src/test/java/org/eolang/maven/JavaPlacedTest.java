/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
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
 *
 * @since 0.56.7
 */
@ExtendWith(MktmpResolver.class)
final class JavaPlacedTest {

    @Test
    void placesJavaGeneratedCode(@Mktmp final Path temp) throws Exception {
        final Path target = temp.resolve("target").resolve("Foo.java");
        final String expected = "public final class Main {}";
        final Path generated = temp.resolve("generated-sources");
        final Xnav java = new Xnav(new Xembler(new Directives().add("java").set(expected)).xml());
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
    @SuppressWarnings("JTCOP.RuleNotContainsTestWord")
    void placesJavaTests(@Mktmp final Path temp) throws Exception {
        final String expected = String.join(
            "\n",
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
                target.resolve("generated-test-sources").resolve("FooTest.java")
            ).asString(),
            Matchers.equalTo(expected)
        );
    }
}
