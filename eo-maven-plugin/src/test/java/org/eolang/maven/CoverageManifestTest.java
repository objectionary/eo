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
import java.nio.file.Paths;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link CoverageManifest}.
 * @since 0.58
 */
@ExtendWith(MktmpResolver.class)
final class CoverageManifestTest {

    @Test
    void derivesItsPathFromTheHitsFile(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the manifest must live next to the hits file, with a .manifest suffix",
            new CoverageManifest(temp.resolve("hits.txt")).path(),
            Matchers.equalTo(Paths.get(String.format("%s.manifest", temp.resolve("hits.txt"))))
        );
    }

    @Test
    void extractsOneLinePerPhCoverageWrapper(@Mktmp final Path temp) throws Exception {
        final CoverageManifest manifest = new CoverageManifest(temp.resolve("hits.txt"));
        manifest.scan(
            String.join(
                System.lineSeparator(),
                "rb1 = new PhCoverage(rb1, \"Φ.foo.x.main\", 5, 1);",
                "rb2 = new PhCoverage(rb2, \"Φ.foo.x.main.α0\", 6, 3);",
                "rb3 = something.else(rb3);"
            )
        );
        MatcherAssert.assertThat(
            "one manifest line must be written per PhCoverage wrapper found",
            new TextOf(manifest.path()).asString(),
            Matchers.allOf(
                Matchers.containsString("Φ.foo.x.main\t5"),
                Matchers.containsString("Φ.foo.x.main.α0\t6")
            )
        );
    }

    @Test
    void discardsAPreviousManifestOnReset(@Mktmp final Path temp) throws IOException {
        final CoverageManifest manifest = new CoverageManifest(temp.resolve("hits.txt"));
        manifest.scan("x = new PhCoverage(x, \"Φ.main\", 1, 1);");
        manifest.reset();
        MatcherAssert.assertThat(
            "reset must discard whatever a previous run wrote",
            Files.exists(manifest.path()),
            Matchers.is(false)
        );
    }
}
