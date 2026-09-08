/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.file.Path;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Bodies}.
 *
 * <p>The programs here are made of several bodies, or of a body that
 * reads a helper in place, so they run the real binary and hold only
 * when it is installed and of the pinned version.</p>
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class BodiesTest {

    @Test
    void resumesHelpersApplyingEachOther(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "two helpers applying each other in tail positions must become two more bodies",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='ξ.a🌵3-4'><o as='α0' base='ξ.x'/>",
                        BodiesTest.number("α1", "3F-F0-00-00-00-00-00-00"),
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8,
                "bounce",
                BodiesTest.bouncers()
            ).program().bodies(),
            Matchers.hasSize(3)
        );
    }

    @Test
    void refusesHelpersThatNeverAnswer(@Mktmp final Path temp) {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        final Map<String, Xnav> helpers = new LinkedHashMap<>();
        helpers.put(
            "a🌵3-4",
            BodiesTest.bouncer(
                "a🌵3-4",
                "<o base='ξ.ρ.a🌵8-4'><o as='α0' base='ξ.n'/><o as='α1' base='ξ.acc'/></o>"
            )
        );
        helpers.put(
            "a🌵8-4",
            BodiesTest.bouncer(
                "a🌵8-4",
                "<o base='ξ.ρ.a🌵3-4'><o as='α0' base='ξ.n'/><o as='α1' base='ξ.acc'/></o>"
            )
        );
        MatcherAssert.assertThat(
            "helpers that only resume each other never answer and must refuse, but they didnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                new Reduction(
                    phino,
                    new Xnav(
                        "<o base='ξ.a🌵3-4'><o as='α0' base='ξ.x'/><o as='α1' base='ξ.x'/></o>"
                    ).element("o"),
                    Collections.singletonMap("x", "number"),
                    8,
                    "bounce",
                    helpers
                )::program,
                "a program that never answers reduced, but it must not"
            ).getMessage(),
            Matchers.containsString("never answers")
        );
    }

    @Test
    void appliesHelperFormationTwice(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a helper with a void must be applied to each argument it is handed, but it isnt",
            new Reduction(
                phino,
                new Xnav(
                    String.join(
                        "",
                        "<o base='.plus'>",
                        "<o base='ξ.a🌵3-4'>",
                        BodiesTest.number("α0", "40-00-00-00-00-00-00-00"),
                        "</o>",
                        "<o as='α0' base='ξ.a🌵3-4'>",
                        BodiesTest.number("α0", "40-08-00-00-00-00-00-00"),
                        "</o>",
                        "</o>"
                    )
                ).element("o"),
                Collections.singletonMap("x", "number"),
                8,
                "",
                Collections.singletonMap(
                    "a🌵3-4",
                    new Xnav(
                        String.join(
                            "",
                            "<o name='a🌵3-4'><o base='∅' name='ρ'/><o base='∅' name='i'/>",
                            "<o base='ξ.ρ.x.times' name='φ'><o as='α0' base='ξ.i'/></o></o>"
                        )
                    ).element("o")
                )
            ).protocol().moves(),
            Matchers.hasSize(3)
        );
    }

    @Test
    void foldsHelperNamedTwiceIntoOneStep(@Mktmp final Path temp) throws Exception {
        final Phino phino = new Phino("phino", 1000, temp);
        Assumptions.assumeTrue(phino.suitable());
        MatcherAssert.assertThat(
            "a helper named twice must be read in place and cost one step, but it doesnt",
            new Reduction(
                phino,
                new Xnav("<o base='ξ.a🌵3-4.plus'><o as='α0' base='ξ.a🌵3-4'/></o>").element("o"),
                Collections.singletonMap("x", "number"),
                8,
                "",
                Collections.singletonMap(
                    "a🌵3-4",
                    new Xnav("<o base='ξ.x.times'><o as='α0' base='ξ.x'/></o>").element("o")
                )
            ).protocol().moves(),
            Matchers.hasSize(2)
        );
    }

    private static Map<String, Xnav> bouncers() {
        final Map<String, Xnav> out = new LinkedHashMap<>();
        out.put(
            "a🌵3-4",
            BodiesTest.bouncer(
                "a🌵3-4",
                String.join(
                    "",
                    "<o base='.if'>",
                    "<o base='ξ.n.eq'>",
                    BodiesTest.number("α0", "00-00-00-00-00-00-00-00"),
                    "</o>",
                    "<o as='α0' base='ξ.acc'/>",
                    "<o as='α1' base='ξ.ρ.a🌵8-4'>",
                    "<o as='α0' base='ξ.n.plus'>",
                    BodiesTest.number("α0", "BF-F0-00-00-00-00-00-00"),
                    "</o>",
                    "<o as='α1' base='ξ.acc.plus'>",
                    BodiesTest.number("α0", "3F-F0-00-00-00-00-00-00"),
                    "</o>",
                    "</o>",
                    "</o>"
                )
            )
        );
        out.put(
            "a🌵8-4",
            BodiesTest.bouncer(
                "a🌵8-4",
                String.join(
                    "",
                    "<o base='.if'>",
                    "<o base='ξ.n.eq'>",
                    BodiesTest.number("α0", "00-00-00-00-00-00-00-00"),
                    "</o>",
                    "<o as='α0' base='ξ.acc'/>",
                    "<o as='α1' base='ξ.ρ.a🌵3-4'>",
                    "<o as='α0' base='ξ.n.plus'>",
                    BodiesTest.number("α0", "BF-F0-00-00-00-00-00-00"),
                    "</o>",
                    "<o as='α1' base='ξ.acc.times'>",
                    BodiesTest.number("α0", "40-00-00-00-00-00-00-00"),
                    "</o>",
                    "</o>",
                    "</o>"
                )
            )
        );
        return out;
    }

    private static Xnav bouncer(final String name, final String body) {
        return new Xnav(
            String.format(
                "<o name='%s'><o base='∅' name='ρ'/><o base='∅' name='n'/><o base='∅' name='acc'/>%s</o>",
                name, body.replaceFirst("<o base=", "<o name='φ' base=")
            )
        ).element("o");
    }

    private static String number(final String name, final String hex) {
        return String.format(
            "<o as='%s' base='Φ.number'><o as='α0' base='Φ.bytes'><o as='α0'>%s</o></o></o>",
            name, hex
        );
    }
}
