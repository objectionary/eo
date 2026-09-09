/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

/**
 * Test case for {@link Fragment}.
 *
 * @since 0.76.0
 */
final class FragmentTest {

    @Test
    void refusesACallOfTheEnclosingObject() {
        final Map<String, String> rows = new HashMap<>();
        rows.put("Φ.foo.calc.φ.ρ.ρ", "Φ.number");
        rows.put("Φ.foo.calc.φ.ρ.α0", "Φ.number");
        MatcherAssert.assertThat(
            "a method with arguments called on the enclosing object must not be carved, but it is",
            new Fragment(
                (Element) new Xnav(
                    String.join(
                        "",
                        "<o base='.plus' loc='Φ.foo.calc.φ'>",
                        "<o base='ξ.ρ.step' loc='Φ.foo.calc.φ.ρ'>",
                        "<o as='α0' base='ξ.x' loc='Φ.foo.calc.φ.ρ.α0'/></o>",
                        "<o as='α0' base='Φ.number'><o base='Φ.bytes'>",
                        "<o>40-00-00-00-00-00-00-00</o></o></o>",
                        "</o>"
                    )
                ).element("o").node(),
                new Formas(rows, Collections.emptyMap())
            ).carved(),
            Matchers.is(false)
        );
    }

    @Test
    void keepsAReadOfTheEnclosingObject() {
        final Map<String, String> rows = new HashMap<>();
        rows.put("Φ.foo.calc.φ.ρ.ρ", "Φ.number");
        rows.put("Φ.foo.calc.φ.ρ.α0", "Φ.number");
        MatcherAssert.assertThat(
            "a plain attribute of the enclosing object must stay a leaf, but it doesnt",
            new Fragment(
                (Element) new Xnav(
                    String.join(
                        "",
                        "<o base='.plus' loc='Φ.foo.calc.φ'>",
                        "<o base='ξ.ρ.x.times' loc='Φ.foo.calc.φ.ρ'>",
                        "<o as='α0' base='ξ.ρ.y' loc='Φ.foo.calc.φ.ρ.α0'/></o>",
                        "<o as='α0' base='Φ.number'><o base='Φ.bytes'>",
                        "<o>40-00-00-00-00-00-00-00</o></o></o>",
                        "</o>"
                    )
                ).element("o").node(),
                new Formas(rows, Collections.emptyMap())
            ).carved(),
            Matchers.is(true)
        );
    }
}
