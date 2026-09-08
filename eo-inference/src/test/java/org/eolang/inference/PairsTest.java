/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Pairs}.
 *
 * @since 0.69.0
 */
final class PairsTest {

    @Test
    void carriesThroughEveryRowThatIsNotAPair() {
        MatcherAssert.assertThat(
            "a row a later pass does not rebuild must survive it, but it didnt",
            new Types(
                new Pairs(
                    new XMLDocument(
                        String.join(
                            "",
                            "<links><type id='a'><terminator/></type>",
                            "<type id='b'><ref loc='x'/></type></links>"
                        )
                    )
                ).others()
            ).asXml(),
            XhtmlMatchers.hasXPath("/links/type[@id='a']/terminator")
        );
    }

    @Test
    void gathersTheVoidsFilledAlongTheWholeChain() {
        MatcherAssert.assertThat(
            "a copy of a copy keeps what the earlier one filled, but it forgot it",
            new Pairs(
                new XMLDocument(
                    String.join(
                        "",
                        "<links><type id='half'><ref loc='pair'>",
                        "<bind void='pair.x'><ref loc='u'/></bind></ref></type>",
                        "<type id='full'><ref loc='half'>",
                        "<bind void='pair.y'><ref loc='v'/></bind></ref></type></links>"
                    )
                )
            ).filled().get("full"),
            Matchers.containsInAnyOrder("pair.y", "pair.x")
        );
    }
}
