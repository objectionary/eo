/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;
import java.util.stream.Stream;
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
        final Raws raws = this.raws(temp);
        final TjForeign tojo = this.saved(temp, "[] > foo%n");
        MatcherAssert.assertThat(
            "the second reader of one source must be given the tree of the first, but it wasnt",
            raws.of(tojo).toString(),
            Matchers.equalTo(raws.of(tojo).toString())
        );
    }

    @Test
    void readsAgainWhenTheTextChanges(@Mktmp final Path temp) throws IOException {
        final Raws raws = this.raws(temp);
        raws.of(this.saved(temp, "[] > foo%n"));
        MatcherAssert.assertThat(
            "a rewritten source must be parsed again, but the tree of its earlier text came back",
            raws.of(this.saved(temp, "[] > foo%n  42 > bar%n")).toString(),
            Matchers.containsString("bar")
        );
    }

    @Test
    void keepsOneTreeWhateverTheNumberOfEdits(@Mktmp final Path temp) throws IOException {
        final long seed = System.nanoTime();
        final Random random = new Random(seed);
        final Raws raws = this.raws(temp);
        final int edits = 2 + random.nextInt(8);
        for (int edit = 0; edit < edits; ++edit) {
            raws.of(
                this.saved(
                    temp,
                    String.format("[] > foo%%n  \"ё%x\" > bar%%n", random.nextLong())
                )
            );
        }
        try (Stream<Path> files = Files.walk(temp.resolve("cache"))) {
            MatcherAssert.assertThat(
                String.format(
                    "the store kept more than one tree after %d edits of one source, seed %d",
                    edits, seed
                ),
                files.filter(file -> file.toString().endsWith(".xmir")).count(),
                Matchers.equalTo(1L)
            );
        }
    }

    private Raws raws(final Path temp) {
        return new Raws(
            new GcShared(temp.resolve("cache"), "1.2.3"), temp.resolve("raw")
        );
    }

    private TjForeign saved(final Path temp, final String text) throws IOException {
        final Path source = temp.resolve("src/foo.eo");
        new Saved(String.format(text), source).value();
        return new TjsForeign().add("foo").withSource(source);
    }
}
