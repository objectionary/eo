/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link org.eolang.maven.LatexMojo}.
 *
 * @since 0.29
 */
@ExtendWith(MktmpResolver.class)
final class LatexMojoTest {

    /**
     * Generate simple main.tex file from main.xmir file
     * and check its existence.
     *
     * @param temp Temporary directory.
     * @throws Exception
     */
    @Test
    void generatesTexFile(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            "LatexMojo must geterate a .tex file, but it doesn't",
            new FakeMaven(temp)
                .withHelloWorld()
                .execute(new FakeMaven.Latex())
                .result(),
            Matchers.hasKey(
                String.format("target/%s/main.%s", LatexMojo.DIR, LatexMojo.EXT)
            )
        );
    }

    /**
     * Checks if the last part of the filename is
     * truncated correctly.
     */
    @Test
    void checksLastName() {
        MatcherAssert.assertThat(
            "Expected the last part of the input, but it was not",
            LatexMojo.last("foo.bar.hello"),
            Matchers.equalTo("hello")
        );
    }

    /**
     * Generate simple main.tex file from main.xmir file
     * and check that there are no DOM variables.
     *
     * @param temp Temporary directory.
     * @throws Exception
     */
    @Test
    void generatesTexFileWithoutDomVars(@Mktmp final Path temp) throws Exception {
        final String output = String.join(
            "\n",
            Files.readAllLines(
                new FakeMaven(temp)
                    .withHelloWorld()
                    .execute(new FakeMaven.Latex())
                    .result()
                    .get(String.format("target/%s/main.%s", LatexMojo.DIR, LatexMojo.EXT))
            )
        );
        MatcherAssert.assertThat(
            "<listing> should not be present in the output",
            output,
            Matchers.not(Matchers.containsString("<listing>"))
        );
        MatcherAssert.assertThat(
            "</listing> should not be present in the output",
            output,
            Matchers.not(Matchers.containsString("</listing>"))
        );
    }
}
