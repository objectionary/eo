/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Signatures}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class SignaturesTest {

    @Test
    void printsTheTypesOfTheVoidsAndOfTheBody(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            String.join(
                "",
                "<provides><type id='Φ.kx.w🌵q'>",
                "<attr name='ρ' void='true' holds='Φ.kx'/>",
                "<attr name='zz' void='true' settled='Φ.string'/>",
                "<attr name='φ' type='Φ.kx.w🌵q.φ'/>",
                "</type></provides>"
            ).getBytes(StandardCharsets.UTF_8)
        );
        Files.write(
            temp.resolve("links.xml"),
            "<links><type id='Φ.kx.w🌵q.φ'><ref loc='Φ.i32'/></type></links>"
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "the signature must show the types that go in and the type that comes out, but it doesnt",
            new Signatures(temp).of("Φ.kx.w🌵q"),
            Matchers.equalTo("w🌵q(Φ.kx, Φ.string)→ Φ.i32")
        );
    }

    @Test
    void takesTheTypeOfTheAtomTheBodyIs(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("links.xml"),
            "<links><type id='Φ.oj.φ'><ref loc='Φ.number.times'/></type></links>"
                .getBytes(StandardCharsets.UTF_8)
        );
        Files.write(
            temp.resolve("atoms.xml"),
            "<atoms><atom forma='Φ.number' loc='Φ.number.times'/></atoms>"
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "the body that is an atom must come out as the type of that atom, but it doesnt",
            new Signatures(temp).of("Φ.oj"),
            Matchers.endsWith("→ Φ.number")
        );
    }

    @Test
    void dropsTheMarkOfAVoidThatMayBeBottom(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            "<provides><type id='Φ.rd'><attr name='y' void='true' holds='Φ.bytes?'/></type></provides>"
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "the type of a void must be shown without its mark of a maybe-⊥, but it isnt",
            new Signatures(temp).of("Φ.rd"),
            Matchers.startsWith("rd(Φ.bytes)")
        );
    }

    @Test
    void marksATypeTheTablesDontKnow(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("provides.xml"),
            "<provides><type id='Φ.ve'><attr name='g' void='true'/></type></provides>"
                .getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "a type the tables dont know must be shown as a question mark, but it isnt",
            new Signatures(temp).of("Φ.ve"),
            Matchers.equalTo("ve(?)→ ?")
        );
    }
}
