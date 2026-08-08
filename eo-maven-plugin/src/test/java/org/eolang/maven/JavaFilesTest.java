/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link JavaFiles}.
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
final class JavaFilesTest {

    @Test
    void countsJavaFilesItWrites(@Mktmp final Path temp) throws IOException {
        final long seed = new Random().nextLong();
        final String name = String.format(
            "bär%d.Ωne%d", Math.abs(seed % 97), Math.abs(seed % 13)
        );
        final Path xmir = temp.resolve("main.xmir");
        new Saved(
            String.format(
                "<object><class java-name='%s'><java>class Ω {}</java></class></object>", name
            ),
            xmir
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the Java files written are not as many as the classes in the XMIR, seed: %d",
                seed
            ),
            new JavaFiles(
                temp.resolve("generated"), temp.resolve("cache").resolve("1.0-SNAPSHOT"), false
            ).total(true, xmir, "", false),
            Matchers.equalTo(1)
        );
    }

    @Test
    void writesExactlyOneFileForAtom(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "only a test class of an atom must be written",
            JavaFilesTest.generateAtom(temp),
            Matchers.equalTo(1)
        );
    }

    @Test
    void skipsAtomImplementationClass(@Mktmp final Path temp) throws IOException {
        JavaFilesTest.generateAtom(temp);
        MatcherAssert.assertThat(
            "an atom implementation class must not be written",
            Files.exists(temp.resolve("generated/EOatom.java")),
            Matchers.equalTo(false)
        );
    }

    @Test
    void writesCompanionClassForAtom(@Mktmp final Path temp) throws IOException {
        JavaFilesTest.generateAtom(temp);
        MatcherAssert.assertThat(
            "an atom test class must be written",
            Files.exists(temp.resolve("generated/EOatomTest.java")),
            Matchers.equalTo(true)
        );
    }

    /**
     * Transpile a single-atom XMIR and report how many files were written.
     * @param temp Temp directory to write into
     * @return Number of files written
     * @throws IOException If fails to save or transpile
     */
    private static int generateAtom(final Path temp) throws IOException {
        final Path xmir = temp.resolve("main.xmir");
        new Saved(
            String.join(
                "",
                "<object><o><o name='λ'/></o>",
                "<class java-name='EOatom'><java>class EOatom {}</java></class>",
                "<class java-name='EOatomTest'><java>class EOatomTest {}</java></class>",
                "</object>"
            ),
            xmir
        ).value();
        return new JavaFiles(
            temp.resolve("generated"), temp.resolve("cache").resolve("1.0-SNAPSHOT"), false
        ).total(true, xmir, "", false);
    }
}
