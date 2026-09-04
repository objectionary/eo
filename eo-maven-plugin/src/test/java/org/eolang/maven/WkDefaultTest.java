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
import org.cactoos.list.ListOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link WkDefault}.
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class WkDefaultTest {

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
            new WkDefault(temp).includes(new ListOf<>(pattern)),
            Matchers.iterableWithSize(count)
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void skipsAnEntryThatIsNotARegularFile(@Mktmp final Path temp) throws Exception {
        Assumptions.assumeTrue(
            WkDefaultTest.fifo(temp.resolve("blocked.eo")),
            "mkfifo is not available here, can't test"
        );
        MatcherAssert.assertThat(
            "a named pipe must not be walked, since reading it waits for a writer that never comes",
            new WkDefault(temp),
            Matchers.emptyIterable()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void skipsALinkWithNothingAtTheEndOfIt(@Mktmp final Path temp) throws IOException {
        Files.createSymbolicLink(temp.resolve("dangling.eo"), temp.resolve("gone.eo"));
        MatcherAssert.assertThat(
            "a link to a missing file must not be walked, since no source can be read through it",
            new WkDefault(temp),
            Matchers.emptyIterable()
        );
    }

    @Test
    @DisabledOnOs(OS.WINDOWS)
    void keepsALinkToARegularFile(@Mktmp final Path temp) throws Exception {
        new Saved("[] > foo", temp.resolve("foo.eo")).value();
        Files.createSymbolicLink(temp.resolve("bar.eo"), temp.resolve("foo.eo"));
        MatcherAssert.assertThat(
            "a link to an ordinary file reads exactly like the file it names, so it must still be walked",
            new WkDefault(temp),
            Matchers.iterableWithSize(2)
        );
    }

    private static boolean fifo(final Path path) throws Exception {
        boolean made;
        try {
            made = new ProcessBuilder("mkfifo", path.toString()).start().waitFor() == 0;
        } catch (final IOException ex) {
            made = false;
        }
        return made;
    }
}
