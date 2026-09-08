/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Applies}.
 * @since 0.72.0
 */
final class AppliesTest {

    @Test
    void gathersACallMadeOnANameRootedAtTheVoid() {
        MatcherAssert.assertThat(
            "the call made a step out of the void must stand on its row, but it didnt",
            new XMLDocument(
                new Xembler(
                    new Directives().add("attr").append(
                        new Applies(
                            Collections.singletonList(
                                new Call("Φ.inc.x.plus", 0, new Ref("Φ.number"))
                            ),
                            new Rooted(Collections.singletonList("Φ.inc.x"))
                        ).directives()
                    )
                ).xmlQuietly()
            ).nodes("/attr/apply[@of='Φ.inc.x.plus' and @place='0']/ref[@loc='Φ.number']"),
            Matchers.hasSize(1)
        );
    }

    @Test
    void leavesAloneACallMadeOnAnotherVoid() {
        MatcherAssert.assertThat(
            "a call made on another void must stay there, but it came here",
            new Applies(
                Collections.singletonList(new Call("Φ.dec.y", 0, new Ref("Φ.number"))),
                new Rooted(Collections.singletonList("Φ.inc.x"))
            ).any(),
            Matchers.is(false)
        );
    }
}
