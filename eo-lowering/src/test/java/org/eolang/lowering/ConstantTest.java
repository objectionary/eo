/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Constant}.
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class ConstantTest {

    @Test
    void foldsDivisionOfNumbers(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the quotient of 42.5 and 2.0 must fold to the bytes of 21.25, but it didnt",
            new Constant(
                phino,
                new Xnav(
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
                ).element("o")
            ).value().bytes(),
            Matchers.equalTo("40-35-00-00-00-00-00-00")
        );
    }

    @Test
    void foldsZeroByZeroIntoNan(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the quotient of zero and zero must fold to the bytes of nan, whatever NaN the host makes, but it didnt",
            new Constant(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.div'>",
                        "<o base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>00-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>00-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o")
            ).value().bytes(),
            Matchers.equalTo("7F-F8-00-00-00-00-00-00")
        );
    }

    @Test
    void namesFormaOfComparison(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a fragment led by gt must carry a bool, but this one doesnt",
            new Constant(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.gt'>",
                        "<o base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-45-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o")
            ).value().forma(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void foldsSizeOfText(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the size of a four-byte text must fold to the bytes of 4, but it didnt",
            new Constant(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.size'>",
                        "<o base='Φ.string'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>D0-B4-D1-80</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o")
            ).value().bytes(),
            Matchers.equalTo("40-10-00-00-00-00-00-00")
        );
    }

    @Test
    void foldsTextHoldingAnEscape(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a text with a line break must equal its own bytes, but it didnt",
            new Constant(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.eq'>",
                        "<o base='Φ.string'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>65-0A-65-0A-65</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>65-0A-65-0A-65</o></o>",
                        "</o>"
                    )
                ).element("o")
            ).value().bytes(),
            Matchers.equalTo("01-")
        );
    }

    @Test
    void refusesTextSlicing(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Constant(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.slice'>",
                        "<o base='Φ.string'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>D0-B4-D1-80</o></o>",
                        "</o>",
                        "<o as='α0' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>00-00-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "<o as='α1' base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                        "</o>",
                        "</o>"
                    )
                ).element("o")
            )::value,
            "slicing a text counts characters, so the byte atom must not fold it, but it did"
        );
    }

    @Test
    void refusesForeignMethod(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        Assertions.assertThrows(
            IllegalStateException.class,
            new Constant(
                phino,
                new Xnav("<o base='.as-i64'><o base='Φ.true'/></o>").element("o")
            )::value,
            "a method the universe does not hold cannot fold, but it did"
        );
    }
}
