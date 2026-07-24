/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
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
    void extractsOneLinePerLocatedElement(@Mktmp final Path temp) throws Exception {
        final CoverageManifest manifest = new CoverageManifest(temp.resolve("hits.txt"));
        manifest.scan(
            new XMLDocument(
                String.join(
                    System.lineSeparator(),
                    "<program>",
                    "  <o line='5' pos='1' loc='Φ.foo.x.main'/>",
                    "  <o line='6' pos='3' loc='Φ.foo.x.main.α0'/>",
                    "  <o line='7' pos='2' loc='Φ.foo.x.main.α0+1'/>",
                    "  <o loc='Φ.foo.x.main.α1'/>",
                    "</program>"
                )
            )
        );
        MatcherAssert.assertThat(
            "one manifest line must be written per located element found, skipping ones without both a line and a position and ones whose locator carries a nested-attribute suffix",
            new TextOf(manifest.path()).asString(),
            Matchers.allOf(
                Matchers.containsString("Φ.foo.x.main\t5"),
                Matchers.containsString("Φ.foo.x.main.α0\t6"),
                Matchers.not(Matchers.containsString("Φ.foo.x.main.α0+1")),
                Matchers.not(Matchers.containsString("Φ.foo.x.main.α1"))
            )
        );
    }

    @Test
    void discardsAPreviousManifestOnReset(@Mktmp final Path temp) throws IOException {
        final CoverageManifest manifest = new CoverageManifest(temp.resolve("hits.txt"));
        manifest.scan(
            new XMLDocument("<program><o line='1' pos='1' loc='Φ.main'/></program>")
        );
        manifest.reset();
        MatcherAssert.assertThat(
            "reset must discard whatever a previous run wrote",
            Files.exists(manifest.path()),
            Matchers.is(false)
        );
    }
}
