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

/**
 * Test case for {@link PhCoverage}.
 * @since 0.58
 */
@ExtendWith(MktmpResolver.class)
final class PhCoverageTest {

    @Test
    void recordsEveryLocationExactlyOnce(@Mktmp final Path temp) throws Exception {
        final Path hits = temp.resolve("hits.txt");
        System.setProperty("eo.coverageFile", hits.toString());
        final Phi first = new PhCoverage(new Data.ToPhi(42L), "Φ.foo", 7, 3);
        new Dataized(first).take();
        new Dataized(first.copy()).take();
        new Dataized(new PhCoverage(new Data.ToPhi(1L), "Φ.bar", 9, 5)).take();
        MatcherAssert.assertThat(
            "every location, hit repeatedly and through a copy, must be recorded exactly once",
            Files.readAllLines(hits, StandardCharsets.UTF_8),
            Matchers.containsInAnyOrder("Φ.foo:7:3", "Φ.bar:9:5")
        );
    }
}
