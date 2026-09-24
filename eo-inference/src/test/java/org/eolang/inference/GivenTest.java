/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Given}.
 *
 * @since 0.69.0
 */
final class GivenTest {

    @Test
    void placesAnInlineBindingByItsIndexNotItsPosition() {
        MatcherAssert.assertThat(
            "an argument bound with :1 must land on the second void, not the first",
            new Given(
                List.of(
                    new XMLDocument(
                        "<o loc='Φ.app.only'><o as='α1' loc='Φ.app.only.α1'/></o>"
                    ).nodes("/o").get(0)
                )
            ).arguments().get("Φ.app.only"),
            Matchers.equalTo(List.of("", "Φ.app.only.α1"))
        );
    }

    @Test
    void keepsPlainApplicationsInDocumentOrder() {
        MatcherAssert.assertThat(
            "two ordinary arguments must keep filling the voids in order",
            new Given(
                List.of(
                    new XMLDocument(
                        String.join(
                            "",
                            "<o loc='Φ.app.pair'>",
                            "<o as='α0' loc='Φ.app.pair.α0'/>",
                            "<o as='α1' loc='Φ.app.pair.α1'/>",
                            "</o>"
                        )
                    ).nodes("/o").get(0)
                )
            ).arguments().get("Φ.app.pair"),
            Matchers.equalTo(List.of("Φ.app.pair.α0", "Φ.app.pair.α1"))
        );
    }

    @Test
    void keepsAnArgumentBoundByNameApartFromThePositionalOnes() {
        MatcherAssert.assertThat(
            "an argument bound by name must be kept, not thrown away by the α filter",
            new Given(
                List.of(
                    new XMLDocument(
                        String.join(
                            "",
                            "<o loc='Φ.app.only'>",
                            "<o as='y' loc='Φ.app.only.y'/>",
                            "<o as='x' loc='Φ.app.only.x'/>",
                            "</o>"
                        )
                    ).nodes("/o").get(0)
                )
            ).named().get("Φ.app.only"),
            Matchers.equalTo(Map.of("y", "Φ.app.only.y", "x", "Φ.app.only.x"))
        );
    }
}
