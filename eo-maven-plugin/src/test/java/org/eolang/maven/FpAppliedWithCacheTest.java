/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
    void appliesWithCache(@Mktmp final Path temp) throws IOException {
        final Path cached = temp.resolve("cached");
        final Path target = temp.resolve("target");
        new FpAppliedWithCache(
            new FpGenerated(input -> new InputOf(new TextOf("testing!"))),
            () -> cached,
            new RewritePolicy(true, temp),
            true
        ).apply(temp.resolve("src"), target);
        MatcherAssert.assertThat(
            "Cached does not have the same content as generated",
            Files.readString(cached),
            Matchers.equalTo(Files.readString(target))
        );
    }

    @Test
    void appliesOnlyToTarget(@Mktmp final Path temp) throws IOException {
        final String expected = "foo bar";
        final Path target = temp.resolve("target");
        new FpAppliedWithCache(
            new FpGenerated(input -> new InputOf(new TextOf(expected))),
            () -> temp.resolve("cached"),
            new RewritePolicy(true, temp),
            false
        ).apply(temp.resolve("src"), target);
        MatcherAssert.assertThat(
            "Cached does not have the same content as generated",
            Files.readString(target),
            Matchers.equalTo(expected)
        );
    }
}
