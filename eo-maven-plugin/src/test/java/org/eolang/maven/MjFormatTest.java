/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
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
    void readsBackTheTreeTheParseGoalLeft(@Mktmp final Path temp) throws Exception {
        final String canonical = MjFormatTest.canonical(new HelloWorld().asString());
        MatcherAssert.assertThat(
            "the source must survive a check that prints from the tree the parse goal wrote",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(canonical)
                    .execute(MjParse.class)
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
                    .with("autofix", true)
                    .withProgram(MjFormatTest.divergent(new HelloWorld().asString()))
                    .execute(MjFormat.class)
                    .result()
                    .get("foo/x/main.eo")
            ).asString(),
            Matchers.equalTo(MjFormatTest.canonical(new HelloWorld().asString()))
        );
    }

    @Test
    void reformatsEverySourceOfABatch(@Mktmp final Path temp) throws Exception {
        final int total = 24;
        final String canonical = MjFormatTest.canonical(new HelloWorld().asString());
        final String divergent = MjFormatTest.divergent(new HelloWorld().asString());
        final FakeMaven maven = new FakeMaven(temp).with("autofix", true);
        for (int idx = 0; idx < total; ++idx) {
            maven.withProgram(divergent);
        }
        final Map<String, Path> result = maven.execute(MjFormat.class).result();
        final Collection<String> formatted = new ArrayList<>(total);
        for (int idx = 0; idx < total; ++idx) {
            formatted.add(
                new TextOf(
                    result.get(String.format("foo/x/main%s.eo", FakeMaven.suffix(idx)))
                ).asString()
            );
        }
        MatcherAssert.assertThat(
            "every source of the batch must be rewritten into its canonical form",
            formatted,
            Matchers.everyItem(Matchers.equalTo(canonical))
        );
    }

    @Test
    void failsWhenSourceDoesNotParse(@Mktmp final Path temp) {
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(MjFormatTest.unparsable())
                .execute(MjFormat.class),
            "a source that fails to parse must not be silently formatted"
        );
        final StringWriter writer = new StringWriter();
        exception.printStackTrace(new PrintWriter(writer));
        MatcherAssert.assertThat(
            "the failure must explain that the source does not fully parse",
            writer.toString(),
            Matchers.containsString("does not fully parse")
        );
    }

    @Test
    void failsWhenErrorRecoveredWithPlaceholder(@Mktmp final Path temp) {
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(MjFormatTest.placeholder())
                .execute(MjFormat.class),
            "a source recovered with a placeholder node must not be silently formatted"
        );
        final StringWriter writer = new StringWriter();
        exception.printStackTrace(new PrintWriter(writer));
        MatcherAssert.assertThat(
            "the failure must explain that the source does not fully parse",
            writer.toString(),
            Matchers.containsString("does not fully parse")
        );
    }

    @Test
    void failsWhenErrorRecoveredByDroppingABinding(@Mktmp final Path temp) {
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(MjFormatTest.droppedBinding())
                .execute(MjFormat.class),
            "a source recovered by dropping a whole binding must not be silently formatted"
        );
        final StringWriter writer = new StringWriter();
        exception.printStackTrace(new PrintWriter(writer));
        MatcherAssert.assertThat(
            "the failure must explain that the source does not fully parse",
            writer.toString(),
            Matchers.containsString("does not fully parse")
        );
    }

    @Test
    void doesNotOverwriteDroppedBindingRecoveryWhenAutoFixIsOn(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .with("autofix", true)
                .withProgram(MjFormatTest.droppedBinding())
                .execute(MjFormat.class),
            "a source the parser only recovered by dropping a binding must not be rewritten"
        );
    }

    @Test
    void doesNotOverwritePlaceholderRecoveryWhenAutoFixIsOn(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .with("autofix", true)
                .withProgram(MjFormatTest.placeholder())
                .execute(MjFormat.class),
            "a source the parser only recovered with a placeholder must not be rewritten"
        );
    }

    private static String canonical(final String program) throws IOException {
        return new Xmir(new EoSyntax(program).parsed()).toEO();
    }

    private static String unparsable() {
        return String.join(
            System.lineSeparator(),
            "+package foo.x",
            "",
            "[x] > main",
            "  (stdout \"Hello!\" x.print > @",
            ""
        );
    }

    private static String placeholder() {
        return String.join(
            System.lineSeparator(),
            "+package foo.x",
            "",
            "[] > foo",
            "  if. > @",
            "    if.",
            "    true",
            "    1",
            "    2",
            ""
        );
    }

    private static String divergent(final String program) throws IOException {
        return String.format("%s%n%n", MjFormatTest.canonical(program));
    }

    private static String droppedBinding() {
        return String.join(
            System.lineSeparator(),
            "+package foo.x",
            "",
            "[x] > foo",
            "  seq * > @",
            "    last",
            "    last.plus 1",
            "  x! > last",
            "",
            "[] > bar",
            "  42 > @",
            ""
        );
    }
}
