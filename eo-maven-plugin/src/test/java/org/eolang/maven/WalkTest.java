/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
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
    void throwsExceptionOnBadGlobs(@Mktmp final Path temp) throws IOException {
        new Saved("", temp.resolve("foo/app/0.1/EOfoo/foo.bin")).value();
        new Saved("", temp.resolve("EOxxx/foo")).value();
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Walk(temp).includes(new ListOf<>("{foo")),
            "Exception must be thrown for invalid glob pattern"
        );
    }

    @Test
    void compilesOnceEagerlyInEmptyDir(@Mktmp final Path empty) {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Walk(empty).includes(new ListOf<>("{eager")),
            "Exception must be thrown for invalid glob pattern"
        );
    }
}
