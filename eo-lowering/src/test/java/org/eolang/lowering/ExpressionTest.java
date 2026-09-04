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
 * Test case for {@link Expression}.
 *
 * <p>The translation is phino's, so these tests need the real binary
 * and skip without it, the same way {@link ConstantTest} does. The
 * {@code plus()} fragment they share is the XMIR of {@code 1.plus 2},
 * a dispatch over two literals.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class ExpressionTest {

    @Test
    void rendersDispatchOnLiterals(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the fragment must become the φ of the root formation, but it didnt",
            new Expression(
                phino, this.plus()
            ).text().replaceAll("\\s+", " "),
            Matchers.containsString(
                String.join(
                    "",
                    "φ ↦ Φ.number( as-bytes ↦ Φ.bytes(",
                    " data ↦ ⟦ Δ ⤍ 3F-F0-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) ).plus(",
                    " α0 ↦ Φ.number( as-bytes ↦ Φ.bytes(",
                    " data ↦ ⟦ Δ ⤍ 40-00-00-00-00-00-00-00, ρ ↦ ∅ ⟧ ) ) )"
                )
            )
        );
    }

    @Test
    void bindsArgumentSiteToPhi(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a site carved from an argument must lose its binding name, but it didnt",
            new Expression(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o as='α0' base='.not'>",
                        "<o base='Φ.bytes'><o as='α0'>01-</o></o>",
                        "</o>"
                    )
                ).element("o")
            ).text().replaceAll("\\s+", " "),
            Matchers.containsString("φ ↦ Φ.bytes( α0 ↦ ⟦ Δ ⤍ 01-, ρ ↦ ∅ ⟧ ).not")
        );
    }

    @Test
    void dataizesWhatItRendered(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "the rendered expression must dataize against the universe, but it didnt",
            phino.dataize(
                new Universe().text(),
                new Expression(
                    phino, this.plus()
                ).text()
            ).bytes(),
            Matchers.equalTo("40-08-00-00-00-00-00-00")
        );
    }

    private Xnav plus() {
        return new Xnav(
            String.join(
                "",
                "<o base='.plus'>",
                "<o base='Φ.number'>",
                "<o as='α0' base='Φ.bytes'><o as='α0'>3F-F0-00-00-00-00-00-00</o></o>",
                "</o>",
                "<o as='α0' base='Φ.number'>",
                "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                "</o>",
                "</o>"
            )
        ).element("o");
    }
}
