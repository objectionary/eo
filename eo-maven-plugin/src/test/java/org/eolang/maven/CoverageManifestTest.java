/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link CoverageManifest}.
 * @since 0.62.0
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
    void extractsOneLinePerLocatedElement(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "one line must be found per located element, skipping the ones without both a line and a position and the ones whose locator carries a nested-attribute suffix",
            CoverageManifestTest.scanned(
                temp,
                String.join(
                    System.lineSeparator(),
                    "<object><class line='5' pos='0' loc='Φ.foo'>",
                    "  <attr name='φ'><bound>",
                    "    <o base='ξ.n' line='5' pos='1' loc='Φ.foo.x'>",
                    "      <o base='.hello' line='6' pos='3' loc='Φ.foo.x.α0'>",
                    "        <o base='Φ.number' line='6' pos='4' loc='Φ.foo.x.α0.α0'/>",
                    "      </o>",
                    "      <o base='Φ.string' line='7' pos='2' loc='Φ.foo.x.α1+1'/>",
                    "      <o base='Φ.bytes' loc='Φ.foo.x.α2'/>",
                    "    </o>",
                    "  </bound></attr>",
                    "</class></object>"
                )
            ),
            Matchers.containsInAnyOrder(
                "Φ.foo.x\t5",
                "Φ.foo.x.α0\t6",
                "Φ.foo.x.α0.α0\t6"
            )
        );
    }

    @Test
    void ignoresElementsThatNeverGetAWrapper(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a formation, a void placeholder and a childless method call carry a line, a position and a locator too, but to-java.xsl emits no PhCoverage around any of them, so counting them here would inflate the reported number of instrumented lines",
            CoverageManifestTest.scanned(
                temp,
                String.join(
                    System.lineSeparator(),
                    "<object><class line='3' pos='0' loc='Φ.foo'>",
                    "  <attr name='n'><void>",
                    "    <o base='∅' line='3' pos='1' loc='Φ.foo.n'/>",
                    "  </void></attr>",
                    "  <attr name='λ'><atom>",
                    "    <o loc='Φ.foo.λ'/>",
                    "    <o base='∅' line='4' pos='6' loc='Φ.foo.λ.x'/>",
                    "  </atom></attr>",
                    "  <attr name='φ'><bound>",
                    "    <o line='5' pos='2' loc='Φ.foo.φ'>",
                    "      <o base='.tail' line='6' pos='3' loc='Φ.foo.φ.α0'/>",
                    "    </o>",
                    "  </bound></attr>",
                    "</class></object>"
                )
            ),
            Matchers.emptyIterable()
        );
    }

    @Test
    void savesEveryLineAtOnce(@Mktmp final Path temp) throws Exception {
        final CoverageManifest manifest = new CoverageManifest(temp.resolve("hits.txt"));
        manifest.save(Arrays.asList("Φ.main\t5", "Φ.main.α0\t6"));
        MatcherAssert.assertThat(
            "every collected line must land in the manifest, one per line",
            new TextOf(manifest.path()).asString(),
            Matchers.allOf(
                Matchers.containsString("Φ.main\t5"),
                Matchers.containsString("Φ.main.α0\t6")
            )
        );
    }

    @Test
    void discardsWhatAPreviousRunLeftBehind(@Mktmp final Path temp) throws Exception {
        final CoverageManifest manifest = new CoverageManifest(temp.resolve("hits.txt"));
        manifest.save(Collections.singletonList("Φ.gone\t1"));
        manifest.save(Collections.singletonList("Φ.fresh\t2"));
        MatcherAssert.assertThat(
            "the second run must replace the manifest of the first one, not append to it",
            new TextOf(manifest.path()).asString(),
            Matchers.not(Matchers.containsString("Φ.gone"))
        );
    }

    /**
     * Every manifest line the given XMIR yields.
     * @param temp Temporary directory to keep the hits file in
     * @param xmir The XMIR, as it stands right before {@code to-java.xsl}
     * @return The lines found
     */
    private static Collection<String> scanned(final Path temp, final String xmir) {
        final Collection<String> found = new ArrayList<>(0);
        new CoverageManifest(temp.resolve("hits.txt")).located(
            new XMLDocument(xmir), one -> one, found
        );
        return found;
    }
}
