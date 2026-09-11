/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Raws}.
 *
 * @since 0.62.0
 */
@ExtendWith(MktmpResolver.class)
final class RawsTest {

    @Test
    void readsTheSameTreeTwice(@Mktmp final Path temp) throws IOException {
        final Raws raws = RawsTest.raws(temp);
        final Path source = RawsTest.saved(temp, "[] > foo%n");
        MatcherAssert.assertThat(
            "the second reader of one source must be given the tree of the first, but it wasnt",
            raws.of("foo", source).toString(),
            Matchers.equalTo(raws.of("foo", source).toString())
        );
    }

    @Test
    void readsAgainWhenTheTextChanges(@Mktmp final Path temp) throws IOException {
        final Raws raws = RawsTest.raws(temp);
        raws.of("foo", RawsTest.saved(temp, "[] > foo%n")).toString();
        MatcherAssert.assertThat(
            "a rewritten source must be parsed again, but the tree of its earlier text came back",
            raws.of("foo", RawsTest.saved(temp, "[] > foo%n  42 > bar%n")).toString(),
            Matchers.containsString("bar")
        );
    }

    private static Raws raws(final Path temp) {
        return new Raws(
            new GcShared(temp.resolve("cache"), "1.2.3"), temp.resolve("raw")
        );
    }

    private static Path saved(final Path temp, final String text) throws IOException {
        final Path source = temp.resolve("src/foo.eo");
        new Saved(String.format(text), source).value();
        return source;
    }
}
