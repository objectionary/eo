/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.PosixFilePermissions;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Phino}.
 *
 * <p>The tests that run the real binary hold only when it is installed
 * and of the pinned version, which is what CI arranges; a machine
 * without it skips them, the same way the goal that uses this class
 * skips its work.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class PhinoTest {

    @Test
    void readsPin(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the pinned version must come from the phino-version.txt resource, but it didnt",
            new Phino("phino", 7, temp).pin(),
            Matchers.matchesPattern("\\d+\\.\\d+\\.\\d+")
        );
    }

    @Test
    void distrustsAbsentBinary(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "an executable that is not there cannot be suitable, but it was",
            new Phino(temp.resolve("no-such-phino").toString(), 7, temp).suitable(),
            Matchers.is(false)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void distrustsWrongVersion(@Mktmp final Path temp) throws Exception {
        final Path fake = temp.resolve("phino");
        Files.write(
            fake,
            String.format("#!/bin/sh%necho 9.9.9%n").getBytes(StandardCharsets.UTF_8)
        );
        Files.setPosixFilePermissions(
            fake, PosixFilePermissions.fromString("rwxr-xr-x")
        );
        MatcherAssert.assertThat(
            "an executable of another version cannot be suitable, but it was",
            new Phino(fake.toString(), 7, temp).suitable(),
            Matchers.is(false)
        );
    }

    @Test
    void dataizesDatum(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 100, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the bytes of a Δ formation must come back verbatim, but they didnt",
            phino.dataize("⟦ Δ ⤍ 2A- ⟧"),
            Matchers.equalTo("2A-")
        );
    }

    @Test
    void refusesUndataizableDocument(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 100, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> phino.dataize("⟦ φ ↦ Φ.miracle ⟧"),
            "a document that never reaches data cannot dataize quietly, but it did"
        );
    }
}
