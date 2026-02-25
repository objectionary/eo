/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link DepDirs}.
 *
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class DepDirsTest {

    @Test
    void findsDirs(@Mktmp final Path temp) throws IOException {
        new Saved("", temp.resolve("a/b/c/f/test.txt")).value();
        new Saved("", temp.resolve("a/b/f.txt")).value();
        new Saved("", temp.resolve("test/f.txt")).value();
        new Saved("", temp.resolve("a/g")).value();
        final String path = String.format("a%sb%1$sc%1$sf", File.separator);
        MatcherAssert.assertThat(
            String.format("DepDirs should contain %s, but it doesn't", path),
            new DepDirs(temp),
            Matchers.allOf(
                Matchers.<String>iterableWithSize(1),
                Matchers.contains(path)
            )
        );
    }

}
