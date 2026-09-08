/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.Together;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
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

    @Test
    void framesEveryDigestField(@Mktmp final Path temp) throws IOException {
        final String locator = "Q.φ";
        final Path dir = temp.resolve("framed");
        Files.createDirectories(dir);
        final Path table = dir.resolve("provides.xml");
        Files.writeString(
            table,
            String.format(
                "<provides><type id=\"%s\"><attr name=\"x\"/></type></provides>",
                locator
            )
        );
        final String value = String.format(
            "provides.xml:%s",
            new XMLDocument(table).nodes("/*/type[@id]").get(0)
        );
        MatcherAssert.assertThat(
            "each locator and row value must carry its UTF-8 byte length into the digest",
            new Rows(dir).digest(Collections.singletonList(locator)),
            Matchers.equalTo(
                new Hashed(
                    String.format(
                        "%d\0%s%d\0%s",
                        locator.getBytes(StandardCharsets.UTF_8).length,
                        locator,
                        value.getBytes(StandardCharsets.UTF_8).length,
                        value
                    )
                ).get()
            )
        );
    }

    @Test
    void answersEveryThreadThatAsksAtOnce(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            temp.resolve("provides.xml"),
            "<provides><type id=\"Q.foo\"><attr name=\"x\"/></type></provides>"
        );
        final Rows rows = new Rows(temp);
        MatcherAssert.assertThat(
            "threads that ask for a digest at the same moment must all be answered, one wasnt",
            new Together<>(30, thread -> rows.digest(Collections.singletonList("Q.foo"))).asList(),
            Matchers.everyItem(
                Matchers.equalTo(new Rows(temp).digest(Collections.singletonList("Q.foo")))
            )
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
