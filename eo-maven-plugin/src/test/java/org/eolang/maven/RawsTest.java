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
    void readsTheTreeOfASource(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the tree of the source must hold the object the source declares, but it didnt",
            RawsTest.tree(temp, "[] > foo\n").toString(),
            Matchers.containsString("foo")
        );
    }

    @Test
    void readsTheSameTreeTwice(@Mktmp final Path temp) throws IOException {
        final Raws raws = new Raws(
            new GcShared(temp.resolve("cache"), "1.2.3"), temp.resolve("raw")
        );
        final Path source = temp.resolve("src/foo.eo");
        new Saved("[] > foo\n", source).value();
        MatcherAssert.assertThat(
            "the second reader of one source must be given the tree of the first, but it wasnt",
            raws.of("foo", source).toString(),
            Matchers.equalTo(raws.of("foo", source).toString())
        );
    }

    @Test
    void doesNotHandOutTheTreeOfAnEarlierText(@Mktmp final Path temp) throws IOException {
        final Raws raws = new Raws(
            new GcShared(temp.resolve("cache"), "1.2.3"), temp.resolve("raw")
        );
        final Path source = temp.resolve("src/foo.eo");
        new Saved("[] > foo\n", source).value();
        raws.of("foo", source).toString();
        new Saved("[] > foo\n  42 > bar\n", source).value();
        MatcherAssert.assertThat(
            "a rewritten source must be parsed again, but the tree of its earlier text came back",
            raws.of("foo", source).toString(),
            Matchers.containsString("bar")
        );
    }

    private static com.jcabi.xml.XML tree(final Path temp, final String text) throws IOException {
        final Path source = temp.resolve("src/foo.eo");
        new Saved(text, source).value();
        return new Raws(
            new GcShared(temp.resolve("cache"), "1.2.3"), temp.resolve("raw")
        ).of("foo", source);
    }
}
