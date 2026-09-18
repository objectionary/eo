/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjDeferred;
import com.yegor256.tojos.Tojos;
import java.io.IOException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Grouped}.
 *
 * @since 0.67.0
 */
final class GroupedTest {

    @Test
    void nestsAttributeUnderItsOwner() throws IOException {
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            rows.add("Φ.jar").set("complete", "true");
            rows.add("Φ.jar honey")
                .set("owner", "Φ.jar")
                .set("name", "honey")
                .set("type", "Φ.jar.honey");
            MatcherAssert.assertThat(
                "an attribute must be nested under the type that owns it, but it isnt",
                new Grouped(rows, "provides").asXml(),
                XhtmlMatchers.hasXPath(
                    "/provides/type[@id='Φ.jar']/attr[@name='honey' and @type='Φ.jar.honey']"
                )
            );
        }
    }

    @Test
    void keepsPlaceOfRowOutOfDocument() throws IOException {
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            rows.add("Φ.hive").set("complete", "true");
            rows.add("Φ.hive comb").set("owner", "Φ.hive").set("name", "comb");
            MatcherAssert.assertThat(
                "the nesting already tells the owner and the row id, so they must not be spelled again",
                new Grouped(rows, "provides").asXml(),
                XhtmlMatchers.hasXPath("/provides/type/attr[not(@owner) and not(@id)]")
            );
        }
    }

    @Test
    void keepsAttributesInOrderTheyWereWritten() throws IOException {
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            rows.add("Φ.pot").set("complete", "true");
            rows.add("Φ.pot lid").set("owner", "Φ.pot").set("name", "lid");
            rows.add("Φ.pot spout").set("owner", "Φ.pot").set("name", "spout");
            MatcherAssert.assertThat(
                "the attributes must follow the code, since a void is bound by its place",
                new Grouped(rows, "provides").asXml(),
                XhtmlMatchers.hasXPath(
                    "/provides/type[attr[1][@name='lid'] and attr[2][@name='spout']]"
                )
            );
        }
    }

    @Test
    void rendersUnknownCellAsAttribute() throws IOException {
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            rows.add("Φ.bee").set("complete", "true").set("mood", "busy");
            MatcherAssert.assertThat(
                "a cell this view has never heard of must still reach the document, but it didnt",
                new Grouped(rows, "provides").asXml(),
                XhtmlMatchers.hasXPath("/provides/type[@id='Φ.bee' and @mood='busy']")
            );
        }
    }

    @Test
    void namesDocumentAfterTable() throws IOException {
        try (Tojos rows = new TjDeferred(new MnMemory())) {
            rows.add("Φ.nest").set("complete", "true");
            MatcherAssert.assertThat(
                "the document must be named after the table it shows, but it isnt",
                new Grouped(rows, "needs").asXml(),
                XhtmlMatchers.hasXPath("/needs/type[@id='Φ.nest']")
            );
        }
    }
}
