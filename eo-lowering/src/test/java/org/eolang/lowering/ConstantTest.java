/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Constant}.
 * @since 0.76.0
 */
final class ConstantTest {

    @Test
    void foldsDivisionOfNumbers() throws Exception {
        final Phino phino = new Phino("phino", 1000);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the quotient of 42.5 and 2.0 must fold to the bytes of 21.25, but it didnt",
            new Constant(
                phino,
                new XMLDocument(
                    String.join(
                        "",
                        "<o base='.div'>",
                        "<o base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-45-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                )
            ).value(),
            Matchers.equalTo("40-35-00-00-00-00-00-00")
        );
    }

    @Test
    void namesFormaOfComparison() {
        MatcherAssert.assertThat(
            "a fragment led by gt must carry a bool, but this one doesnt",
            new Constant(
                new Phino("phino", 7),
                new XMLDocument("<o base='.gt'><o base='Φ.true'/></o>")
            ).forma(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void refusesForeignMethod() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Constant(
                new Phino("phino", 7),
                new XMLDocument("<o base='.as-i64'><o base='Φ.true'/></o>")
            ).forma(),
            "a method outside the twelve primitives cannot name a forma, but it did"
        );
    }
}
