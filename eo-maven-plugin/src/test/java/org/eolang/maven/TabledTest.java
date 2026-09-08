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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Tabled}.
 *
 * @since 0.71.0
 */
@ExtendWith(MktmpResolver.class)
final class TabledTest {

    @Test
    void countsTheTablesAndTheirWeight(@Mktmp final Path temp) throws IOException {
        Files.writeString(temp.resolve("provides.xml"), "<provides/>");
        Files.writeString(temp.resolve("links.xml"), "<links/>");
        MatcherAssert.assertThat(
            "both tables must be counted, with their bytes together",
            new Tabled(temp).asString(),
            Matchers.equalTo("2 table(s), 19b total")
        );
    }

    @Test
    void saysNothingIsThereWhenTheDirectoryIsAbsent(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a directory nobody wrote must be counted as empty, not blow up",
            new Tabled(temp.resolve("absent")).asString(),
            Matchers.startsWith("0 table(s)")
        );
    }
}
