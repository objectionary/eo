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
        MjLowerTest.assumePhino();
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
        MjLowerTest.assumePhino();
        MatcherAssert.assertThat(
            "the object must be transpiled from the folded XMIR and not from the parsed one",
            MjLowerTest.lowered(temp).foreignTojos().find("foo").xmir().toString(),
            Matchers.containsString(Lowering.DIR)
        );
    }

    @Test
    void writesTheMarker(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino();
        MatcherAssert.assertThat(
            "a run that folded must say so in the marker file, but it didnt",
            Files.readString(MjLowerTest.marker(MjLowerTest.lowered(temp))),
            Matchers.startsWith("lower-")
        );
    }

    @Test
    void leavesContextDependentExpressionAlone(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino();
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
    void failsWithoutTheBinaryWhenDemanded(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(MjLowerTest.constant(), "foo", "foo.eo")
                .with("binary", temp.resolve("no-such-phino").toString())
                .with("demanded", true)
                .execute(new PpLower()),
            "a missing binary under eo.loweringRequired cannot pass quietly, but it did"
        );
    }

    @Test
    void removesTheMarkerWhenDisabled(@Mktmp final Path temp) throws IOException {
        MjLowerTest.assumePhino();
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

    private static void assumePhino() {
        Assumptions.assumeTrue(new Phino("phino", 7).suitable());
    }

    private static Path marker(final FakeMaven maven) {
        return maven.targetPath().resolve(Lowering.DIR).resolve(Lowering.MARKER);
    }

    private static FakeMaven lowered(final Path temp) throws IOException {
        return new FakeMaven(temp)
            .withProgram(MjLowerTest.constant(), "foo", "foo.eo")
            .execute(new PpLower());
    }

    private static String constant() {
        return MjLowerTest.program("[] > foo", "  1.plus 1 > @");
    }

    private static String program(final String... lines) {
        return String.join(System.lineSeparator(), lines);
    }
}
