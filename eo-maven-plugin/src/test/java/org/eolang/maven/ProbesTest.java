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
 * @since 0.53
 */
final class ProbesTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/probe-packs/", glob = "**.yaml")
    void checksProbePacks(final String yaml) throws IOException {
        final Xtory xtory = new XtSticky(new XtYaml(yaml));
        final List<String> expected = Optional.ofNullable(
            (List<String>) xtory.map().get("probes")
        ).orElse(Collections.emptyList());
        MatcherAssert.assertThat(
            "Probes should match the ones in the YAML file",
            new Probes(new EoSyntax(xtory.map().get("eo").toString()).parsed()),
            Matchers.allOf(
                Matchers.<String>iterableWithSize(expected.size()),
                Matchers.containsInAnyOrder(expected.toArray(new String[0]))
            )
        );
    }

    @Test
    void findsProbes() throws IOException {
        MatcherAssert.assertThat(
            "We should find 8 objects in the program, but not",
            new Probes(ProbesTest.xmir()),
            Matchers.allOf(
                Matchers.iterableWithSize(8),
                Matchers.hasItems(
                    "while",
                    "io",
                    "io.stdout",
                    "string.sprintf",
                    "string",
                    "bytes",
                    "number",
                    "number.plus"
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
                    System.lineSeparator(),
                    "[] > simple",
                    "  Q.io.stdout > @",
                    "    \"Hello, world!\""
                )
            ).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
        MatcherAssert.assertThat(
            "We should find 6 objects in simple program, but not",
            new Probes(xmir),
            Matchers.allOf(
                Matchers.iterableWithSize(4),
                Matchers.hasItems(
                    "io",
                    "io.stdout",
                    "string",
                    "bytes"
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
                        System.lineSeparator(),
                        String.format("+alias Q.io.stdout%n"),
                        "stdout \"Hello, world!\" > simple"
                    )
                ).parsed()
            ),
            Matchers.allOf(
                Matchers.iterableWithSize(4),
                Matchers.hasItems(
                    "io",
                    "io.stdout",
                    "string",
                    "bytes"
                )
            )
        );
    }

    private static XML xmir() throws IOException {
        return new EoSyntax(
            String.join(
                System.lineSeparator(),
                new String[]{
                    "+package foo.x",
                    String.format("+also while%n"),
                    "[] > main",
                    "  Q.io.stdout > @",
                    "    Q.string.sprintf",
                    "      \"I am %d years old\"",
                    "      plus.",
                    "        1337",
                    "        228",
                }
            )
        ).parsed();
    }
}
