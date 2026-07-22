/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Test case for {@link PhCoverage}.
 *
 * <p>Isolated because it drives the JVM-wide {@code eo.coverageFile}
 * property, which every {@code PhCoverage} touches: when coverage
 * instrumentation is on, a concurrent test would append its own hits to
 * this test's file and break the exact-contents assertion.</p>
 *
 * @since 0.58
 */
@Isolated
@ExtendWith(MktmpResolver.class)
final class PhCoverageTest {

    @Test
    void staysEqualToItsOrigin() {
        final Phi origin = new Data.ToPhi(42L);
        MatcherAssert.assertThat(
            "a coverage wrapper must compare equal to the object it wraps, but it didnt",
            new PhCoverage(origin, "Φ.n", 1, 1),
            Matchers.equalTo(origin)
        );
    }

    @Test
    void recordsEveryLocationExactlyOnce(@Mktmp final Path temp) throws Exception {
        final Path hits = temp.resolve("hits.txt");
        final String before = System.getProperty("eo.coverageFile");
        System.setProperty("eo.coverageFile", hits.toString());
        try {
            final Phi first = new PhCoverage(
                new PhDefault(new byte[] {(byte) 0x2A}), "Φ.foo", 7, 3
            );
            new Dataized(first).take();
            new Dataized(first.copy()).take();
            new Dataized(
                new PhCoverage(new PhDefault(new byte[] {(byte) 0x01}), "Φ.bar", 9, 5)
            ).take();
            MatcherAssert.assertThat(
                "every location, hit repeatedly and through a copy, must be recorded exactly once",
                Files.readAllLines(hits, StandardCharsets.UTF_8),
                Matchers.containsInAnyOrder("Φ.foo:7:3", "Φ.bar:9:5")
            );
        } finally {
            if (before == null) {
                System.clearProperty("eo.coverageFile");
            } else {
                System.setProperty("eo.coverageFile", before);
            }
        }
    }
}
