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
            ).value(),
            Matchers.equalTo("40-35-00-00-00-00-00-00")
        );
    }

    @Test
    void namesFormaOfComparison(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a fragment led by gt must carry a bool, but this one doesnt",
            new Constant(
                new Phino("phino", 7, temp),
                new Xnav("<o base='.gt'><o base='Φ.true'/></o>").element("o")
            ).forma(),
            Matchers.equalTo("bool")
        );
    }

    @Test
    void refusesForeignMethod(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Constant(
                new Phino("phino", 7, temp),
                new Xnav("<o base='.as-i64'><o base='Φ.true'/></o>").element("o")
            )::forma,
            "a method outside the twelve primitives cannot name a forma, but it did"
        );
    }
}
