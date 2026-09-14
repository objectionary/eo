/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * Test case for {@link Route}.
 *
 * @since 0.77.0
 */
final class RouteTest {

    @Test
    void climbsOneRhoPerFormationBetween() {
        final Element fragment = RouteTest.fragment(
            "<o name='g'><o base='ξ.x' name='h'><o as='α0' base='Φ.number' name='site'/></o></o>"
        );
        MatcherAssert.assertThat(
            "a void must be reached up one ρ per formation, not per application, but it isnt",
            new Route(fragment, RouteTest.site(fragment)).to("ρ.a"),
            Matchers.equalTo("ξ.ρ.ρ.a")
        );
    }

    @Test
    void reachesFormationFromCommonAncestor() {
        final Element fragment = RouteTest.fragment("<o base='Φ.number' name='site'/>");
        MatcherAssert.assertThat(
            "a formation beside the fragment must be reached from their common parent, but it isnt",
            new Route(fragment, RouteTest.site(fragment)).to("box:Φ.foo.g"),
            Matchers.equalTo("ξ.ρ.g")
        );
    }

    @Test
    void namesForeignFormationByLocator() {
        final Element fragment = RouteTest.fragment("<o base='Φ.number' name='site'/>");
        MatcherAssert.assertThat(
            "a formation outside the top-level object must be named by its locator, but it isnt",
            new Route(fragment, RouteTest.site(fragment)).to("box:Φ.bar.g"),
            Matchers.equalTo("Φ.bar.g")
        );
    }

    private static Element fragment(final String body) {
        return (Element) new Xnav(
            String.format(
                "<object><o loc='Φ.foo' name='foo'><o loc='Φ.foo.f' name='f'>%s</o></o></object>",
                body
            )
        ).element("object").element("o").element("o").node();
    }

    private static Element site(final Element fragment) {
        final NodeList all = fragment.getElementsByTagName("o");
        Element out = fragment;
        for (int idx = 0; idx < all.getLength(); ++idx) {
            if ("site".equals(((Element) all.item(idx)).getAttribute("name"))) {
                out = (Element) all.item(idx);
            }
        }
        return out;
    }
}
