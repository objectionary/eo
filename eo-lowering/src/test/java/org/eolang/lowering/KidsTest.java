/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.util.ArrayList;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.w3c.dom.Element;

/**
 * Test case for {@link Kids}.
 *
 * @since 0.77.0
 */
final class KidsTest {

    @Test
    void listsOnlyElementsNamedO() {
        final List<String> names = new ArrayList<>(2);
        for (final Element kid
            : new Kids(
                new Xnav("<object><o name='a'/>txt<!--c--><x/><o name='b'/></object>")
                    .element("object").node()
            )) {
            names.add(kid.getAttribute("name"));
        }
        MatcherAssert.assertThat(
            "only the o elements must be listed, in order, but they arent",
            names,
            Matchers.contains("a", "b")
        );
    }
}
