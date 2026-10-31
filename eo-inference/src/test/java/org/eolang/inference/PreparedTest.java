/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import java.io.IOException;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Prepared}.
 *
 * @since 0.76.0
 */
final class PreparedTest {

    @Test
    void splitsChainDownToBareHead() throws IOException {
        MatcherAssert.assertThat(
            "every name of a chain must be a read of its own, the first one too, but it wasnt",
            new Prepared().apply(
                new EoSyntax(
                    String.join(System.lineSeparator(), "[x] > box", "  x.lid > @", "")
                ).parsed()
            ),
            XhtmlMatchers.hasXPath("//o[@base='.lid']/o[@base='.x']/o[@base='ξ']")
        );
    }

    @Test
    void locatesObjectsTheSplitMade() throws IOException {
        MatcherAssert.assertThat(
            "the bare head under a chain must get a locator of its own, but it didnt",
            new Prepared().apply(
                new EoSyntax(
                    String.join(System.lineSeparator(), "[x] > crate", "  x.lid.hinge > @", "")
                ).parsed()
            ),
            XhtmlMatchers.hasXPath("//o[@base='ξ' and @loc='Φ.crate.φ.ρ.ρ.ρ']")
        );
    }
}
