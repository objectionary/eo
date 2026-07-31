/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Walk}.
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class WalkTest {

    @Test
    void findsFilesMatchingGlobPattern(@Mktmp final Path temp) throws Exception {
        new Saved("", temp.resolve("foo/hello/0.1/EObar/x.bin")).value();
        new Saved("", temp.resolve("EOxxx/bar")).value();
        final String pattern = "EO**/*";
        final int count = 1;
        MatcherAssert.assertThat(
            String.format(
                "Expected %d file(s) matching pattern '%s'",
                count,
                pattern
            ),
            new Walk(temp).includes(new ListOf<>(pattern)),
            Matchers.iterableWithSize(count)
        );
    }

    @Test
    void namesTheMalformedGlobInsteadOfLeakingARegexError(@Mktmp final Path temp) throws Exception {
        new Saved("", temp.resolve("EOxxx/bar")).value();
        final String glob = "**/[a-.eo";
        MatcherAssert.assertThat(
            "the message must say the glob comes from source selection, which the JDK's own regex error does not, even though it does quote the pattern",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new Walk(temp).includes(new ListOf<>(glob)),
                "a malformed glob must be reported, not thrown as a raw regex error"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString(glob),
                Matchers.containsString("configured to select sources")
            )
        );
    }
}
