/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Grouped}.
 * @since 0.67.0
 */
final class GroupedTest {

    @Test
    void nestsAttributeUnderItsOwner() {
        MatcherAssert.assertThat(
            "an attribute must be nested under the type that owns it, but it isnt",
            new Grouped(
                Arrays.asList(
                    new Row("Φ.jar").with("complete", "true"),
                    new Row("Φ.jar honey")
                        .with("owner", "Φ.jar")
                        .with("name", "honey")
                        .with("type", "Φ.jar.honey")
                ),
                "provides"
            ).asXml(),
            XhtmlMatchers.hasXPath(
                "/provides/type[@id='Φ.jar']/attr[@name='honey' and @type='Φ.jar.honey']"
            )
        );
    }

    @Test
    void keepsPlaceOfRowOutOfDocument() {
        MatcherAssert.assertThat(
            "the nesting already tells the owner and the row id, so they must not be spelled again",
            new Grouped(
                Arrays.asList(
                    new Row("Φ.hive").with("complete", "true"),
                    new Row("Φ.hive comb").with("owner", "Φ.hive").with("name", "comb")
                ),
                "provides"
            ).asXml(),
            XhtmlMatchers.hasXPath("/provides/type/attr[not(@owner) and not(@id)]")
        );
    }

    @Test
    void keepsAttributesInOrderTheyWereWritten() {
        MatcherAssert.assertThat(
            "the attributes must follow the code, since a void is bound by its place",
            new Grouped(
                Arrays.asList(
                    new Row("Φ.pot").with("complete", "true"),
                    new Row("Φ.pot lid").with("owner", "Φ.pot").with("name", "lid"),
                    new Row("Φ.pot spout").with("owner", "Φ.pot").with("name", "spout")
                ),
                "provides"
            ).asXml(),
            XhtmlMatchers.hasXPath(
                "/provides/type[attr[1][@name='lid'] and attr[2][@name='spout']]"
            )
        );
    }

    @Test
    void rendersUnknownCellAsAttribute() {
        MatcherAssert.assertThat(
            "a cell this view has never heard of must still reach the document, but it didnt",
            new Grouped(
                Collections.singletonList(
                    new Row("Φ.bee").with("complete", "true").with("mood", "busy")
                ),
                "provides"
            ).asXml(),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.bee' and @mood='busy']")
        );
    }

    @Test
    void namesDocumentAfterTable() {
        MatcherAssert.assertThat(
            "the document must be named after the table it shows, but it isnt",
            new Grouped(
                Collections.singletonList(new Row("Φ.nest").with("complete", "true")),
                "needs"
            ).asXml(),
            XhtmlMatchers.hasXPath("/needs/type[@id='Φ.nest']")
        );
    }
}
