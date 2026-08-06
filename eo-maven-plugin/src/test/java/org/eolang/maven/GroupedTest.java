/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.yegor256.tojos.MnMemory;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.Tojos;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Grouped}.
 * @since 0.67.0
 */
final class GroupedTest {

    @Test
    void nestsAttributeUnderItsOwner() {
        final Tojos rows = new TjDefault(new MnMemory());
        rows.add("Φ.jar").set("complete", "true");
        rows.add("Φ.jar honey").set("owner", "Φ.jar").set("name", "honey")
            .set("type", "Φ.jar.honey");
        MatcherAssert.assertThat(
            "an attribute must be nested under the type that owns it, but it isnt",
            new Grouped(rows, "provides").asXml(),
            XhtmlMatchers.hasXPath(
                "/provides/type[@id='Φ.jar']/attr[@name='honey' and @type='Φ.jar.honey']"
            )
        );
    }

    @Test
    void keepsPlaceOfRowOutOfDocument() {
        final Tojos rows = new TjDefault(new MnMemory());
        rows.add("Φ.hive").set("complete", "true");
        rows.add("Φ.hive comb").set("owner", "Φ.hive").set("name", "comb");
        MatcherAssert.assertThat(
            "the nesting already tells the owner and the row id, so they must not be spelled again",
            new Grouped(rows, "provides").asXml(),
            XhtmlMatchers.hasXPath("/provides/type/attr[not(@owner) and not(@id)]")
        );
    }

    @Test
    void rendersUnknownCellAsAttribute() {
        final Tojos rows = new TjDefault(new MnMemory());
        rows.add("Φ.bee").set("complete", "true").set("mood", "busy");
        MatcherAssert.assertThat(
            "a cell this view has never heard of must still reach the document, but it didnt",
            new Grouped(rows, "provides").asXml(),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.bee' and @mood='busy']")
        );
    }

    @Test
    void namesDocumentAfterTable() {
        final Tojos rows = new TjDefault(new MnMemory());
        rows.add("Φ.nest").set("complete", "true");
        MatcherAssert.assertThat(
            "the document must be named after the table it shows, but it isnt",
            new Grouped(rows, "needs").asXml(),
            XhtmlMatchers.hasXPath("/needs/type[@id='Φ.nest']")
        );
    }
}
