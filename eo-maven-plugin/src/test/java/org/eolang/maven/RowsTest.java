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
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Rows}.
 * @since 0.75.0
 */
@ExtendWith(MktmpResolver.class)
final class RowsTest {

    @Test
    void takesTheRowsOfWhatTheObjectHolds(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a row of an attribute of the object must change the digest of the object",
            this.digest(temp.resolve("one"), "<type id=\"Q.foo.phi\"><attr name=\"x\"/></type>"),
            Matchers.not(
                Matchers.equalTo(this.digest(temp.resolve("two"), "<type id=\"Q.foo.phi\"/>"))
            )
        );
    }

    @Test
    void leavesTheRowsOfANeighbourOut(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "an object whose locator merely starts with the same letters is somebody else",
            this.digest(temp.resolve("with"), "<type id=\"Q.foobar\"><attr name=\"x\"/></type>"),
            Matchers.equalTo(this.digest(temp.resolve("without"), ""))
        );
    }

    private String digest(final Path dir, final String rows) throws IOException {
        Files.createDirectories(dir);
        Files.writeString(
            dir.resolve("provides.xml"), String.format("<provides>%s</provides>", rows)
        );
        return new Rows(dir).digest(Collections.singletonList("Q.foo"));
    }
}
