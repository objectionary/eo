/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Pairs}.
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
}
