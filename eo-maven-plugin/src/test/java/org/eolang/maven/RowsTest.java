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
 *
 * @since 0.75.0
 */
@ExtendWith(MktmpResolver.class)
final class RowsTest {

    @Test
    void takesTheRowsOfWhatTheObjectHolds(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a row of an attribute of the object must change the digest of the object, but it didnt",
            this.digest(
                temp.resolve("one"), "<o loc=\"Q.foo.φ\" copies=\"2\"><is loc=\"Q.number\"/></o>"
            ),
            Matchers.not(
                Matchers.equalTo(
                    this.digest(
                        temp.resolve("two"), "<o loc=\"Q.foo.φ\" copies=\"2\"><data/></o>"
                    )
                )
            )
        );
    }

    @Test
    void leavesTheRowsOfANeighbourOut(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a row of an object whose locator merely starts with the same letters changed the digest",
            this.digest(
                temp.resolve("with"), "<o loc=\"Q.foobar\" copies=\"5\"><terminator/></o>"
            ),
            Matchers.equalTo(this.digest(temp.resolve("without"), ""))
        );
    }

    @Test
    void readsPastAHyphenatedSibling(@Mktmp final Path temp) throws IOException {
        final String neighbours = String.join(
            "",
            "<o loc=\"Q.foo\" copies=\"1\"><is loc=\"Q.foo\"/></o>",
            "<o loc=\"Q.foo-bar\" copies=\"1\"><is loc=\"Q.foo-bar\"/></o>"
        );
        MatcherAssert.assertThat(
            "a change in a descendant row must reach the digest, a hyphenated sibling hid it",
            this.digest(
                temp.resolve("one"),
                String.format(
                    "%s<o loc=\"Q.foo.inner\" copies=\"1\" lands=\"x\"><data/></o>", neighbours
                )
            ),
            Matchers.not(
                Matchers.equalTo(
                    this.digest(
                        temp.resolve("two"),
                        String.format(
                            "%s<o loc=\"Q.foo.inner\" copies=\"1\" lands=\"y\"><data/></o>",
                            neighbours
                        )
                    )
                )
            )
        );
    }

    @Test
    void takesTheVoidsOfTheFacts(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void the facts add to a formation must change the digest of the formation, but it didnt",
            this.facts(
                temp.resolve("one"),
                String.join(
                    "",
                    "<facts><objects><type id=\"Q.foo\">",
                    "<attr name=\"x\" type=\"Q.foo.x\" void=\"true\"/>",
                    "</type></objects></facts>"
                )
            ),
            Matchers.not(
                Matchers.equalTo(
                    this.facts(
                        temp.resolve("two"),
                        "<facts><objects><type id=\"Q.foo\"/></objects></facts>"
                    )
                )
            )
        );
    }

    @Test
    void leavesTheHeadsOfTheFactsOut(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a row of the facts outside their objects changed the digest",
            this.facts(
                temp.resolve("heads"),
                "<facts><heads><type id=\"Q.foo.φ\"><ref loc=\"Q\"/></type></heads></facts>"
            ),
            Matchers.equalTo(this.facts(temp.resolve("none"), "<facts/>"))
        );
    }

    @Test
    void framesEveryDigestField(@Mktmp final Path temp) throws IOException {
        final String locator = "Q.φ";
        final Path dir = temp.resolve("framed");
        Files.createDirectories(dir);
        final Path table = dir.resolve("answers.xml");
        Files.writeString(
            table,
            String.format(
                "<answers><o loc=\"%s\" copies=\"4\"><is loc=\"Q.string\"/></o></answers>",
                locator
            )
        );
        final String value = String.format(
            "answers.xml:%s",
            new XMLDocument(table).nodes("/answers/o").get(0)
        );
        MatcherAssert.assertThat(
            "each locator and row value must carry its UTF-8 byte length into the digest, but one didnt",
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
            temp.resolve("answers.xml"),
            "<answers><o loc=\"Q.foo\" copies=\"7\"><is loc=\"Q.foo\"/></o></answers>"
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
            dir.resolve("answers.xml"), String.format("<answers>%s</answers>", rows)
        );
        return new Rows(dir).digest(Collections.singletonList("Q.foo"));
    }

    private String facts(final Path dir, final String table) throws IOException {
        Files.createDirectories(dir);
        Files.writeString(dir.resolve("facts.xml"), table);
        return new Rows(dir).digest(Collections.singletonList("Q.foo"));
    }
}
