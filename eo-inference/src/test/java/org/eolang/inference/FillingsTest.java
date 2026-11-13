/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Fillings}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class FillingsTest {

    @Test
    void takesFormationWrittenAsArgument(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a formation written as an argument must fill its void, but it didnt",
            FillingsTest.fillings(
                temp, "[] > report", "  os.status > @", "    5", "    [^]", "      \"z\" > @"
            ).of("Φ.os.status", "v🌵note"),
            Matchers.contains(new Filling('O', "Φ.report.φ.α1"))
        );
    }

    @Test
    void takesLiteralAsCallOnKindOfCanon(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a literal must fill its void as a call on its kind, but it didnt",
            FillingsTest.fillings(temp, "[] > report", "  os.status 3 \"q\" > @")
                .of("Φ.os.status", "v🌵note"),
            Matchers.contains(new Filling('C', "Φ.string"))
        );
    }

    @Test
    void takesFormationPathReadsFromRoot(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a path that reads a formation from the root must fill its void with it, but it didnt",
            FillingsTest.fillings(
                temp, "[] > report", "  os.status 0 os > @"
            ).of("Φ.os.status", "v🌵note"),
            Matchers.contains(new Filling('O', "Φ.os"))
        );
    }

    @Test
    void takesArgumentThatReadsVoidAsOneOnlyWalkAnswers(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "an argument that reads a void must stand as one only the walk answers, but it didnt",
            FillingsTest.fillings(temp, "[y] > report", "  os.status 1 y > @")
                .of("Φ.os.status", "v🌵note"),
            Matchers.contains(new Filling('W', "Φ.report.φ.α1"))
        );
    }

    @Test
    void landsNamedArgumentInItsVoid(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an argument named by its void must fill that void, but it didnt",
            FillingsTest.fillings(temp, "[] > report", "  os.status > @", "    \"k\":note")
                .of("Φ.os.status", "v🌵note"),
            Matchers.contains(new Filling('C', "Φ.string"))
        );
    }

    @Test
    void leavesVoidNoCallFillsEmpty(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void no call fills must have no fillings, but it had some",
            FillingsTest.fillings(temp, "[] > report", "  os.check > @")
                .of("Φ.os.status", "v🌵code"),
            Matchers.empty()
        );
    }

    private static Fillings fillings(final Path temp, final String... lines)
        throws IOException {
        final Program program = new Sources(temp).with(
            "os.eo",
            "[] > os",
            "  [] > check /Q.os.status",
            "  [code note] > status",
            "    code > @",
            "    note > text"
        ).with("report.eo", lines).program();
        return new Fillings(program, new Values(program));
    }
}
