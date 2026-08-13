/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test for {@link Unhomed}.
 * @since 0.62.0
 */
final class UnhomedTest {

    @Test
    void takesOwnPackageOffHomedReference(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final Path file = tmp.resolve("parsed.xmir");
        new Saved(
            String.format(
                "<object>%n  <metas>%n    <meta>%n      <head>package</head>%n      <part>input</part>%n    </meta>%n  </metas>%n  <o name=\"копия%d\">%n    <o base=\"Φ.input.length\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the package was not taken off the reference the parse goal homed, seed %d", seed
            ),
            new Unhomed(file, "input.length input.tee").tree().xpath("//o[@base]/@base").get(0),
            Matchers.equalTo("Φ.length")
        );
    }

    @Test
    void keepsReferenceThatWasNeverHomed(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final Path file = tmp.resolve("parsed.xmir");
        new Saved(
            String.format(
                "<object>%n  <metas>%n    <meta>%n      <head>package</head>%n      <part>tuple</part>%n    </meta>%n  </metas>%n  <o name=\"назад%d\">%n    <o base=\"Φ.tuple.empty\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the global attribute was mistaken for a package mate and stripped, seed %d", seed
            ),
            new Unhomed(file, "tuple.back tuple.each").tree().xpath("//o[@base]/@base").get(0),
            Matchers.equalTo("Φ.tuple.empty")
        );
    }

    @Test
    void leavesTreeOfProgramWithoutPackageAlone(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final Path file = tmp.resolve("parsed.xmir");
        new Saved(
            String.format(
                "<object>%n  <o name=\"объект%d\">%n    <o base=\"Φ.input.length\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the reference of a program with no package was not left alone, seed %d", seed
            ),
            new Unhomed(file, "input.length").tree().xpath("//o[@base]/@base").get(0),
            Matchers.equalTo("Φ.input.length")
        );
    }
}
