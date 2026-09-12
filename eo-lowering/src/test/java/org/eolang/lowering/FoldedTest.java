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
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Folded}.
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class FoldedTest {

    @Test
    void foldsSumUnderRefusedEquality(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        final Xnav doc = new Xnav(
            String.join(
                "",
                "<object><o base='.eq'>",
                "<o base='.plus'>",
                "<o base='Φ.number'>",
                "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                "</o>",
                "<o as='α0' base='Φ.number'>",
                "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                "</o>",
                "</o>",
                "<o as='α0' base='Φ.number'>",
                "<o as='α0' base='Φ.bytes'><o as='α0'>40-08-00-00-00-00-00-00</o></o>",
                "</o>",
                "</o></object>"
            )
        );
        new Folded(phino).rewrite(doc);
        MatcherAssert.assertThat(
            "the sum under an equality phino refuses must fold on its own, but it didnt",
            doc.element("object").element("o").element("o").attribute("base").text().orElse(""),
            Matchers.equalTo("Φ.number")
        );
    }
}
