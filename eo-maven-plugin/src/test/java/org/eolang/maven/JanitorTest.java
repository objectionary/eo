/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test for {@link Janitor}.
 * @since 0.62.0
 */
@ExtendWith(MktmpResolver.class)
final class JanitorTest {

    @Test
    void doesNothingForReleasedVersion(@Mktmp final Path temp) {
        new Janitor(temp, "1.0").clean();
        MatcherAssert.assertThat(
            "Fingerprint must not be created for released versions",
            Files.exists(temp.resolve(".snapshot-fingerprint")),
            Matchers.is(false)
        );
    }

    @Test
    void wipesStageDirsOnFirstRun(@Mktmp final Path temp) throws Exception {
        final String version = "1.0-SNAPSHOT";
        new Saved("", JanitorTest.staleFile(temp, version)).value();
        new Janitor(temp, version).clean();
        MatcherAssert.assertThat(
            "Stale SNAPSHOT cache must be wiped on first run",
            Files.exists(JanitorTest.staleFile(temp, version)),
            Matchers.is(false)
        );
    }

    @Test
    void writesFingerprintOnFirstRun(@Mktmp final Path temp) throws Exception {
        final String version = "1.0-SNAPSHOT";
        new Saved("", JanitorTest.staleFile(temp, version)).value();
        new Janitor(temp, version).clean();
        MatcherAssert.assertThat(
            "Fingerprint file must be written after wipe",
            Files.exists(temp.resolve(".snapshot-fingerprint")),
            Matchers.is(true)
        );
    }

    @Test
    void doesNothingWhenFingerprintMatches(@Mktmp final Path temp) throws Exception {
        final String version = "1.0-SNAPSHOT";
        new Janitor(temp, version).clean();
        final Path kept = temp.resolve("transpiled").resolve(version).resolve("valid.xmir");
        new Saved("", kept).value();
        new Janitor(temp, version).clean();
        MatcherAssert.assertThat(
            "Cache must not be wiped when fingerprint matches",
            Files.exists(kept),
            Matchers.is(true)
        );
    }

    @Test
    void wipesStageDirsWhenFingerprintDiffers(@Mktmp final Path temp) throws Exception {
        final String version = "1.0-SNAPSHOT";
        Files.write(
            temp.resolve(".snapshot-fingerprint"),
            "outdated-hash".getBytes(StandardCharsets.UTF_8)
        );
        new Saved("", JanitorTest.staleFile(temp, version)).value();
        new Janitor(temp, version).clean();
        MatcherAssert.assertThat(
            "Stale cache must be wiped when fingerprint differs",
            Files.exists(JanitorTest.staleFile(temp, version)),
            Matchers.is(false)
        );
    }

    /**
     * Builds the path to the stale transpiled file used in cache-wipe tests.
     * @param temp Temporary directory
     * @param version Plugin version
     * @return Path to the stale file inside the transpiled stage cache
     */
    private static Path staleFile(final Path temp, final String version) {
        return temp.resolve("transpiled").resolve(version).resolve("stale.xmir");
    }
}
