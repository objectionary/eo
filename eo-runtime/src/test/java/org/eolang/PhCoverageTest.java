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
import java.util.Optional;
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

    @Test
    void convertsInvalidCoveragePathIntoIllegalArgumentException() {
        MatcherAssert.assertThat(
            "a raw InvalidPathException leaked through unconverted",
            this.failure(String.format("/tmp/%cinvalid", (char) 0), "Φ.invalid-path").getClass(),
            Matchers.<Class<?>>equalTo(IllegalArgumentException.class)
        );
    }

    @Test
    void namesThePropertyAndItsValueInAnInvalidCoveragePathFailure() {
        final String bad = String.format("/tmp/%cinvalid", (char) 0);
        MatcherAssert.assertThat(
            "the failure must name the offending property and its value",
            this.failure(bad, "Φ.invalid-value").getMessage(),
            Matchers.allOf(
                Matchers.containsString("eo.coverageFile"),
                Matchers.containsString(bad)
            )
        );
    }

    /**
     * Failure raised while dataizing with the given value in the property.
     *
     * <p>Each caller must pass its own location: {@link PhCoverage} writes a
     * given location at most once per JVM, so a shared one would make the
     * second test see no attempt to write at all.</p>
     *
     * @param path Value to put into the {@code eo.coverageFile} property
     * @param loc Location to record, unique per caller
     * @return The exception thrown, or a placeholder if nothing was thrown
     */
    private RuntimeException failure(final String path, final String loc) {
        final Optional<String> before = Optional.ofNullable(System.getProperty("eo.coverageFile"));
        System.setProperty("eo.coverageFile", path);
        RuntimeException thrown = new IllegalStateException("nothing was thrown at all");
        try {
            new Dataized(
                new PhCoverage(
                    new PhDefault(new byte[] {(byte) 0x03}), loc, 11, 1
                )
            ).take();
        } catch (final IllegalArgumentException ex) {
            thrown = ex;
        } finally {
            System.clearProperty("eo.coverageFile");
            before.ifPresent(value -> System.setProperty("eo.coverageFile", value));
        }
        return thrown;
    }
}
