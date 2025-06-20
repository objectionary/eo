/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests for {@link FpAppliedWithCache}.
 *
 * @since 0.56.7
 */
@ExtendWith(MktmpResolver.class)
final class FpAppliedWithCacheTest {

    @Test
    void appliesThroughCache(@Mktmp final Path temp) throws IOException {
        new FpAppliedWithCache(
            new FpGenerated(input -> new InputOf(new TextOf("testing!"))),
            () -> temp.resolve("cached"),
            new RewritePolicy(true, temp),
            true
        ).apply(temp.resolve("src"), temp.resolve("target"));
        // writes to both
    }

    @Test
    void appliesOnlyToTarget() {
    }
}
