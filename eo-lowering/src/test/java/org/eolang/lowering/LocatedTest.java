/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

/**
 * Test case for {@link Located}.
 *
 * @since 0.77.0
 */
final class LocatedTest {

    @Test
    void findsNestedElementByLocator() {
        MatcherAssert.assertThat(
            "the nested element must be found by its locator, but it wasnt",
            new Located(LocatedTest.root(), "Φ.foo.f.g").element().getAttribute("name"),
            Matchers.equalTo("g")
        );
    }

    @Test
    void refusesAbsentLocator() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Located(LocatedTest.root(), "Φ.foo.q").element(),
            "a locator the document lacks must be refused, but it wasnt"
        );
    }

    private static Element root() {
        return (Element) new Xnav(
            "<object><o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' name='f'><o loc='Φ.foo.f.g' name='g'/></o></o></object>"
        ).element("object").node();
    }
}
