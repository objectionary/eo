/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.lowering.Phino;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjLower}.
 *
 * <p>The tests that fold for real hold only when a phino binary of the
 * pinned version is installed, which is what CI arranges; a machine
 * without it skips them, exactly as the goal itself would skip its
 * work.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class MjLowerTest {

    @Test
    void foldsConstantExpression(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "the sum of two literals must become one literal, but it didnt",
            new XMLDocument(
                MjLowerTest.lowered(temp).foreignTojos().find("foo").xmir()
            ).xpath("/object/o/o[@name='φ']/@base").get(0),
            Matchers.equalTo("Φ.number")
        );
    }

    @Test
    void repointsTheObjectAtTheFoldedXmir(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "the object must be transpiled from the folded XMIR and not from the parsed one",
            MjLowerTest.lowered(temp).foreignTojos().find("foo").xmir().toString(),
            Matchers.containsString(Lowering.DIR)
        );
    }

    @Test
    void writesTheMarker(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "a run that folded must say so in the marker file, but it didnt",
            Files.readString(MjLowerTest.marker(MjLowerTest.lowered(temp))),
            Matchers.startsWith("lower-")
        );
    }

    @Test
    void leavesContextDependentExpressionAlone(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "an expression reading a void cannot fold, so the object must keep its parsed XMIR",
            new FakeMaven(temp)
                .withProgram(MjLowerTest.program("[x] > foo", "  x.plus 1 > @"), "foo", "foo.eo")
                .execute(new PpLower())
                .foreignTojos()
                .find("foo")
                .xmir()
                .toString(),
            Matchers.not(Matchers.containsString(Lowering.DIR))
        );
    }

    @Test
    void transpilesLoweredFormationIntoAtomClass(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "the lowered formation must transpile into its own atom class, but it didnt",
            Files.readString(
                MjLowerTest.symbolic(temp)
                    .execute(new PpLower())
                    .execute(MjTranspile.class)
                    .generatedPath()
                    .resolve("org")
                    .resolve("eolang")
                    .resolve("EOfoo$EObump.java")
            ),
            Matchers.containsString(
                "public final class EOfoo$EObump extends PhDefault implements Atom {"
            )
        );
    }

    @Test
    void splicesSidecarBodyIntoLambda(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "the generated lambda must read the void through the public API, but it doesnt",
            Files.readString(
                MjLowerTest.symbolic(temp)
                    .execute(new PpLower())
                    .execute(MjTranspile.class)
                    .generatedPath()
                    .resolve("org")
                    .resolve("eolang")
                    .resolve("EOfoo$EObump.java")
            ),
            Matchers.containsString(
                "final double v0 = new Dataized(this.take(\"x\")).asNumber();"
            )
        );
    }

    @Test
    void generatesNoAtomClassWhenDisabled(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "a run disabled by eo.lowering must generate no atom class, but it did",
            Files.exists(
                MjLowerTest.symbolic(temp)
                    .with("lowering", false)
                    .execute(new PpLower())
                    .execute(MjTranspile.class)
                    .generatedPath()
                    .resolve("org")
                    .resolve("eolang")
                    .resolve("EOfoo$EObump.java")
            ),
            Matchers.is(false)
        );
    }

    @Test
    void skipsQuietlyWithoutTheBinary(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a machine without phino must build with no marker left behind, but one was left",
            Files.exists(
                MjLowerTest.marker(
                    new FakeMaven(temp)
                        .withProgram(MjLowerTest.constant(), "foo", "foo.eo")
                        .with("binary", temp.resolve("no-such-phino").toString())
                        .execute(new PpLower())
                )
            ),
            Matchers.is(false)
        );
    }

    @Test
    void failsWithoutTheBinaryWhenDemanded(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(MjLowerTest.constant(), "foo", "foo.eo")
            .with("binary", temp.resolve("no-such-phino").toString())
            .with("demanded", true);
        final PpLower pipeline = new PpLower();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(pipeline),
            "a missing binary under eo.loweringRequired cannot pass quietly, but it did"
        );
    }

    @Test
    void skipsQuietlyWhenDisabledEvenWhenDemanded(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "eo.lowering set to false must outrank eo.loweringRequired, but it didnt",
            Files.exists(
                MjLowerTest.marker(
                    new FakeMaven(temp)
                        .withProgram(MjLowerTest.constant(), "foo", "foo.eo")
                        .with("binary", temp.resolve("no-such-phino").toString())
                        .with("lowering", false)
                        .with("demanded", true)
                        .execute(new PpLower())
                )
            ),
            Matchers.is(false)
        );
    }

    @Test
    void removesTheMarkerWhenDisabled(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "a run disabled by eo.lowering must take the marker of an earlier run away, but it didnt",
            Files.exists(
                MjLowerTest.marker(
                    MjLowerTest.lowered(temp)
                        .with("lowering", false)
                        .execute(MjLower.class)
                )
            ),
            Matchers.is(false)
        );
    }

    @Test
    void removesTheMarkerWhenSkipping(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino(temp);
        MatcherAssert.assertThat(
            "a run without phino must take the marker of an earlier fold away, but it didnt",
            Files.exists(
                MjLowerTest.marker(
                    MjLowerTest.lowered(temp)
                        .with("binary", temp.resolve("no-such-phino").toString())
                        .execute(MjLower.class)
                )
            ),
            Matchers.is(false)
        );
    }

    private static void assumePhino(final Path temp) {
        Assumptions.assumeTrue(new Phino("phino", 7, temp).suitable());
    }

    private static Path marker(final FakeMaven maven) {
        return maven.targetPath().resolve(Lowering.DIR).resolve(Lowering.MARKER);
    }

    private static FakeMaven lowered(final Path temp) throws IOException {
        return new FakeMaven(temp)
            .withProgram(MjLowerTest.constant(), "foo", "foo.eo")
            .execute(new PpLower());
    }

    private static FakeMaven symbolic(final Path temp) throws IOException {
        return new FakeMaven(temp).withProgram(
            MjLowerTest.program(
                "[] > foo",
                "  [x] > bump",
                "    (x.times 2).plus 1 > @",
                "  bump 5 > @"
            ),
            "foo", "foo.eo"
        ).withProgram(
            MjLowerTest.program("[as-bytes] > number", "  as-bytes > @"),
            "number", "number.eo"
        );
    }

    private static String constant() {
        return MjLowerTest.program("[] > foo", "  1.plus 1 > @");
    }

    private static String program(final String... lines) {
        return String.join(System.lineSeparator(), lines);
    }
}
