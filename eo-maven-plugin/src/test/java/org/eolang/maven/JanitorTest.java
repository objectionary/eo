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
        new Janitor(temp, "1.0").exec();
        MatcherAssert.assertThat(
            "Fingerprint must not be created for released versions",
            Files.exists(temp.resolve(".snapshot-fingerprint")),
            Matchers.is(false)
        );
    }

    @Test
    void wipesStageDirsAndWritesFingerprintOnFirstRun(@Mktmp final Path temp) throws Exception {
        final String version = "1.0-SNAPSHOT";
        Files.createDirectories(temp.resolve("transpiled").resolve(version));
        Files.createFile(
            temp.resolve("transpiled").resolve(version).resolve("stale.xmir")
        );
        new Janitor(temp, version).exec();
        MatcherAssert.assertThat(
            "Stale SNAPSHOT cache must be wiped on first run",
            Files.exists(temp.resolve("transpiled").resolve(version).resolve("stale.xmir")),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            "Fingerprint file must be written after wipe",
            Files.exists(temp.resolve(".snapshot-fingerprint")),
            Matchers.is(true)
        );
    }

    @Test
    void doesNothingWhenFingerprintMatches(@Mktmp final Path temp) throws Exception {
        final String version = "1.0-SNAPSHOT";
        new Janitor(temp, version).exec();
        Files.createDirectories(temp.resolve("transpiled").resolve(version));
        final Path kept = temp.resolve("transpiled").resolve(version).resolve("valid.xmir");
        Files.createFile(kept);
        new Janitor(temp, version).exec();
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
        Files.createDirectories(temp.resolve("transpiled").resolve(version));
        Files.createFile(
            temp.resolve("transpiled").resolve(version).resolve("stale.xmir")
        );
        new Janitor(temp, version).exec();
        MatcherAssert.assertThat(
            "Stale cache must be wiped when fingerprint differs",
            Files.exists(temp.resolve("transpiled").resolve(version).resolve("stale.xmir")),
            Matchers.is(false)
        );
    }
}
