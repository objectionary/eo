/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import org.eolang.jucs.ClasspathSource;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test cases for {@link Probes}.
 *
 * @since 0.53
 * @todo #4203:90min Enable {@link ProbesTest#findsProbesInSimpleProgram()} Test.
 *  This test is currently disabled because it fails.
 *  We should investigate the cause of the failure and enable the test.
 *  Originally this issue affected the integration tests,
 *  <a href="https://github.com/objectionary/eo/issues/4203">here</a>
 */
final class ProbesTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/probe-packs/", glob = "**.yaml")
    void checksProbePacks(final String yaml) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(yaml));
        final List<String> expected = Optional.ofNullable(
            (List<String>) xtory.map().get("probes")
        ).orElse(Collections.emptyList());
        final Probes actual = new Probes(new EoSyntax(xtory.map().get("eo").toString()).parsed());
        MatcherAssert.assertThat(
            "We should find the same number of probes as in the YAML file",
            actual,
            Matchers.iterableWithSize(expected.size())
        );
        MatcherAssert.assertThat(
            "Probes should match the ones in the YAML file",
            actual,
            Matchers.containsInAnyOrder(expected.toArray(new String[0]))
        );
    }

    @Test
    void findsProbes() throws IOException {
        MatcherAssert.assertThat(
            "We should find 11 objects in the program, but not",
            new Probes(ProbesTest.xmir()),
            Matchers.allOf(
                Matchers.iterableWithSize(11),
                Matchers.hasItems(
                    "org.eolang.while",
                    "org",
                    "org.eolang",
                    "org.eolang.io",
                    "org.eolang.io.stdout",
                    "org.eolang.txt",
                    "org.eolang.txt.sprintf",
                    "org.eolang.string",
                    "org.eolang.bytes",
                    "org.eolang.number",
                    "org.eolang.number.plus"
                )
            )
        );
    }

    @Test
    void findsProbesInFile(@TempDir final Path tmp) throws IOException {
        final Path xmir = tmp.resolve("simple.xmir");
        Files.write(
            xmir,
            new EoSyntax(
                String.join(
                    "\n",
                    "# No comments.",
                    "[] > simple",
                    "  QQ.io.stdout > @",
                    "    \"Hello, world!\""
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "We should find 6 objects in simple program, but not",
            new Probes(xmir),
            Matchers.allOf(
                Matchers.iterableWithSize(6),
                Matchers.hasItems(
                    "org",
                    "org.eolang",
                    "org.eolang.io",
                    "org.eolang.io.stdout",
                    "org.eolang.string",
                    "org.eolang.bytes"
                )
            )
        );
    }

    @Test
    @Disabled
    void findsProbesInSimpleProgram() throws IOException {
        MatcherAssert.assertThat(
            "We should find 6 objects in the program",
            new Probes(
                new EoSyntax(
                    String.join(
                        "\n",
                        "+alias QQ.io.stdout\n",
                        "stdout \"Hello, world!\" > simple"
                    )
                ).parsed()
            ),
            Matchers.iterableWithSize(6)
        );
    }

    private static XML xmir() throws IOException {
        return new EoSyntax(
            String.join(
                "\n",
                new String[]{
                    "+package foo.x",
                    "+also while\n",
                    "# No comments.",
                    "[] > main",
                    "  QQ.io.stdout > @",
                    "    QQ.txt.sprintf",
                    "      \"I am %d years old\"",
                    "      plus.",
                    "        1337",
                    "        228",
                }
            )
        ).parsed();
    }
}
