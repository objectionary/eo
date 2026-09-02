/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
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
 * <p>For the same reason every test that asserts on the contents of that
 * file wraps a plain {@link PhDefault} and never a transpiled object like
 * {@link Data.ToPhi}: with {@code eo.coverageTracking} on, the generated
 * objects are themselves wrapped into {@link PhCoverage}, so dataizing one
 * appends its own locations to the very file the test has just pointed the
 * property at.</p>
 *
 * @since 0.58
 */
@Isolated
@ExtendWith(MktmpResolver.class)
final class PhCoverageTest {

    @Test
    void staysDistinctFromItsOrigin() {
        final Phi origin = new Data.ToPhi(42L);
        MatcherAssert.assertThat(
            "a coverage wrapper must not compare equal to the object it wraps",
            new PhCoverage(origin, "Φ.n:1:1"),
            Matchers.not(Matchers.equalTo(origin))
        );
    }

    @Test
    void recordsEveryLocationExactlyOnce(@Mktmp final Path temp) throws Exception {
        final Path hits = temp.resolve("hits.txt");
        final String before = System.getProperty("eo.coverageFile");
        System.setProperty("eo.coverageFile", hits.toString());
        try {
            final Phi first = new PhCoverage(
                new PhDefault(new byte[] {(byte) 0x2A}), "Φ.foo:7:3"
            );
            new Dataized(first).take();
            new Dataized(first.copy()).take();
            new Dataized(
                new PhCoverage(new PhDefault(new byte[] {(byte) 0x01}), "Φ.bar:9:5")
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
    void recordsALocationOnceTheDestinationBecomesWritable(@Mktmp final Path temp)
        throws Exception {
        final Path hits = temp.resolve("absent").resolve("hits.txt");
        final String before = System.getProperty("eo.coverageFile");
        System.setProperty("eo.coverageFile", hits.toString());
        try {
            final Phi covered = new PhCoverage(
                new PhDefault(new byte[] {(byte) 0x01}), "Φ.retry:4:2"
            );
            Assertions.assertThrows(
                UncheckedIOException.class,
                covered::delta,
                "a hit that cannot be appended must fail loudly, but it didnt"
            );
            Files.createDirectory(hits.getParent());
            covered.delta();
            MatcherAssert.assertThat(
                "a location must be recorded once the destination becomes writable, but it wasnt",
                Files.readAllLines(hits, StandardCharsets.UTF_8),
                Matchers.contains("Φ.retry:4:2")
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
    void recordsALocationTouchedThroughANormalizedObject(@Mktmp final Path temp)
        throws Exception {
        final Path hits = temp.resolve("hits.txt");
        final String before = System.getProperty("eo.coverageFile");
        System.setProperty("eo.coverageFile", hits.toString());
        try {
            new PhCoverage(
                new PhDefault(new byte[] {(byte) 0x01}), "Φ.normalized:3:5"
            ).normalized().delta();
            MatcherAssert.assertThat(
                "a location touched through a normalized object must still be recorded, but it wasnt",
                Files.readAllLines(hits, StandardCharsets.UTF_8),
                Matchers.contains("Φ.normalized:3:5")
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
    void writesToEachConfiguredDestinationOnce(@Mktmp final Path temp) throws Exception {
        final Path first = temp.resolve("first.txt");
        final Path second = temp.resolve("second.txt");
        final String before = System.getProperty("eo.coverageFile");
        final Phi covered = new PhCoverage(
            new PhDefault(new byte[] {(byte) 0x2A}), "Φ.foo:7:3"
        );
        try {
            System.setProperty("eo.coverageFile", first.toString());
            new Dataized(covered).take();
            System.setProperty("eo.coverageFile", second.toString());
            new Dataized(covered).take();
            MatcherAssert.assertThat(
                "the same location must be recorded once per configured destination",
                Stream.concat(
                    Files.readAllLines(first, StandardCharsets.UTF_8).stream(),
                    Files.readAllLines(second, StandardCharsets.UTF_8).stream()
                ).collect(Collectors.toList()),
                Matchers.contains("Φ.foo:7:3", "Φ.foo:7:3")
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
    void recordsTheSameLocationInALaterRun(@Mktmp final Path temp) throws Exception {
        final Path hits = temp.resolve("hits.txt");
        final String before = System.getProperty("eo.coverageFile");
        System.setProperty("eo.coverageFile", hits.toString());
        try {
            new Dataized(
                new PhCoverage(new PhDefault(new byte[] {(byte) 0x2A}), "Φ.später:13:7")
            ).take();
            Files.delete(hits);
            new Dataized(
                new PhCoverage(new PhDefault(new byte[] {(byte) 0x2A}), "Φ.später:13:7")
            ).take();
            MatcherAssert.assertThat(
                "a location an earlier object recorded must be recorded again, but it wasnt",
                Files.readAllLines(hits, StandardCharsets.UTF_8),
                Matchers.contains("Φ.später:13:7")
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
            this.failure(String.format("/tmp/%cinvalid", (char) 0)).getClass(),
            Matchers.<Class<?>>equalTo(IllegalArgumentException.class)
        );
    }

    @Test
    void namesThePropertyAndItsValueInAnInvalidCoveragePathFailure() {
        final String bad = String.format("/tmp/%cinvalid", (char) 0);
        MatcherAssert.assertThat(
            "the failure must name the offending property and its value",
            this.failure(bad).getMessage(),
            Matchers.allOf(
                Matchers.containsString("eo.coverageFile"),
                Matchers.containsString(bad)
            )
        );
    }

    private RuntimeException failure(final String path) {
        final Optional<String> before = Optional.ofNullable(System.getProperty("eo.coverageFile"));
        System.setProperty("eo.coverageFile", path);
        RuntimeException thrown = new IllegalStateException("nothing was thrown at all");
        try {
            new Dataized(
                new PhCoverage(
                    new PhDefault(new byte[] {(byte) 0x03}), "Φ.invalid:11:1"
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
