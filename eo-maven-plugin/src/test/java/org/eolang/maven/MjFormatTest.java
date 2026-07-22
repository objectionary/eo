/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.text.TextOf;
import org.eolang.parser.EoSyntax;
import org.eolang.printer.Xmir;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjFormat}.
 * @since 0.57.0
 */
@ExtendWith(MktmpResolver.class)
final class MjFormatTest {

    @Test
    void passesWhenSourceIsCanonical(@Mktmp final Path temp) throws IOException {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(MjFormatTest.canonical(new HelloWorld().asString()))
                .execute(MjFormat.class),
            "canonical source must pass the format check without failing the build"
        );
    }

    @Test
    void keepsCanonicalSourceUntouched(@Mktmp final Path temp) throws Exception {
        final String canonical = MjFormatTest.canonical(new HelloWorld().asString());
        MatcherAssert.assertThat(
            "the canonical source must be left exactly as it was",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(canonical)
                    .execute(MjFormat.class)
                    .result()
                    .get("foo/x/main.eo")
            ).asString(),
            Matchers.equalTo(canonical)
        );
    }

    @Test
    void failsWhenSourceDiverges(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a divergent source must fail the build in check mode",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new FakeMaven(temp)
                    .withProgram(MjFormatTest.divergent(new HelloWorld().asString()))
                    .execute(MjFormat.class)
            ).getMessage(),
            Matchers.notNullValue()
        );
    }

    @Test
    void reformatsDivergentSourceWhenAutoFixIsOn(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the divergent source must be rewritten into its canonical form",
            new TextOf(
                new FakeMaven(temp)
                    .with("autoFix", true)
                    .withProgram(MjFormatTest.divergent(new HelloWorld().asString()))
                    .execute(MjFormat.class)
                    .result()
                    .get("foo/x/main.eo")
            ).asString(),
            Matchers.equalTo(MjFormatTest.canonical(new HelloWorld().asString()))
        );
    }

    @Test
    void appliesCustomIndentationStep(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "the printer must indent with the configured number of spaces",
            new TextOf(
                new FakeMaven(temp)
                    .with("autoFix", true)
                    .with("step", 4)
                    .withProgram(MjFormatTest.canonical(MjFormatTest.nested()))
                    .execute(MjFormat.class)
                    .result()
                    .get("foo/x/main.eo")
            ).asString(),
            Matchers.containsString(
                String.valueOf('\n').concat("        x > first")
            )
        );
    }

    /**
     * A program that stays multi-line whatever the layout weights are.
     *
     * <p>A nested formation with two named bindings and no {@code φ}
     * decoratee never collapses onto a single line — there is no φ to inline
     * as the head of a compact only-phi form — so its deepest lines sit two
     * indentation levels in and expose the configured {@code step}, unlike a
     * compact one-liner such as {@code (stdout "Hello!" x).print > [x] > main}.
     * </p>
     *
     * @return The EO program source
     */
    private static String nested() {
        return String.join(
            System.lineSeparator(),
            "+package foo.x",
            "",
            "[x] > main",
            "  [] > inner",
            "    x > first",
            "    x > second"
        );
    }

    /**
     * Reformat a program into its canonical EO layout.
     * @param program The EO program
     * @return The canonical EO representation
     * @throws IOException If fails to parse the program
     */
    private static String canonical(final String program) throws IOException {
        return new Xmir(new EoSyntax(program).parsed()).toEO();
    }

    /**
     * A non-canonical variant of the program, with extra blank lines.
     * @param program The EO program
     * @return An EO text that diverges from the canonical layout
     * @throws IOException If fails to parse the program
     */
    private static String divergent(final String program) throws IOException {
        return String.format("%s%n%n", MjFormatTest.canonical(program));
    }
}
