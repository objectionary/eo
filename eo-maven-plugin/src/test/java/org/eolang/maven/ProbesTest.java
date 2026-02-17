/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test cases for {@link Probes}.
 *
 * @since 0.53
 */
final class ProbesTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/probe-packs/", glob = "**.yaml")
    void checksProbePacks(final String yaml) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(yaml));
        MatcherAssert.assertThat(
            "Probes should match the ones in the YAML file",
            new Probes(new EoSyntax(xtory.map().get("eo").toString()).parsed()),
            Matchers.containsInAnyOrder(
                Optional.ofNullable(
                    (List<String>) xtory.map().get("probes")
                ).orElse(Collections.emptyList()).toArray(new String[0])
            )
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
                    "org.eolang.tt",
                    "org.eolang.tt.sprintf",
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
                    "    QQ.tt.sprintf",
                    "      \"I am %d years old\"",
                    "      plus.",
                    "        1337",
                    "        228",
                }
            )
        ).parsed();
    }
}
