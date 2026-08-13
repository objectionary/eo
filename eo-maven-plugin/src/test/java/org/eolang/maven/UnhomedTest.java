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
                "<object>%n  <metas>%n    <meta>%n      <head>package</head>%n      <part>bytes</part>%n    </meta>%n  </metas>%n  <o name=\"hash%d\">%n    <o base=\"Φ.bytes.array\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the program's own package was not taken off the homed reference, seed %d", seed
            ),
            new Unhomed(file).tree().xpath("//o[@base]/@base").get(0),
            Matchers.equalTo("Φ.array")
        );
    }

    @Test
    void keepsReferenceIntoAnotherPackage(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final Path file = tmp.resolve("parsed.xmir");
        new Saved(
            String.format(
                "<object>%n  <metas>%n    <meta>%n      <head>package</head>%n      <part>bytes</part>%n    </meta>%n  </metas>%n  <o name=\"hash%d\">%n    <o base=\"Φ.числа.большое\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the reference into another package did not survive untouched, seed %d", seed
            ),
            new Unhomed(file).tree().xpath("//o[@base]/@base").get(0),
            Matchers.equalTo("Φ.числа.большое")
        );
    }

    @Test
    void leavesTreeOfProgramWithoutPackageAlone(@TempDir final Path tmp) throws IOException {
        final long seed = System.nanoTime();
        final Path file = tmp.resolve("parsed.xmir");
        new Saved(
            String.format(
                "<object>%n  <o name=\"hash%d\">%n    <o base=\"Φ.bytes.array\"/>%n  </o>%n</object>%n",
                seed
            ),
            file
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the reference of a program with no package was not left alone, seed %d", seed
            ),
            new Unhomed(file).tree().xpath("//o[@base]/@base").get(0),
            Matchers.equalTo("Φ.bytes.array")
        );
    }
}
