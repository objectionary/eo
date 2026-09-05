/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Cycles}.
 * @since 0.76.0
 */
final class CyclesTest {

    @Test
    void namesHelpersApplyingEachOther() {
        final Map<String, Xnav> helpers = new LinkedHashMap<>();
        helpers.put("a🌵3-4", CyclesTest.helper("a🌵3-4", "ξ.ρ.a🌵8-4"));
        helpers.put("a🌵8-4", CyclesTest.helper("a🌵8-4", "ξ.ρ.a🌵3-4"));
        MatcherAssert.assertThat(
            "two helpers applying each other must both be in the cycle, but they arent",
            new Cycles(helpers).names(),
            Matchers.contains("a🌵3-4", "a🌵8-4")
        );
    }

    @Test
    void namesHelperApplyingItself() {
        MatcherAssert.assertThat(
            "a helper applying itself must be in the cycle, but it isnt",
            new Cycles(
                Collections.singletonMap("a🌵3-4", CyclesTest.helper("a🌵3-4", "ξ.ρ.a🌵3-4"))
            ).names(),
            Matchers.contains("a🌵3-4")
        );
    }

    @Test
    void leavesOutHelperAppliedOnce() {
        final Map<String, Xnav> helpers = new LinkedHashMap<>();
        helpers.put("a🌵3-4", CyclesTest.helper("a🌵3-4", "ξ.ρ.a🌵8-4"));
        helpers.put("a🌵8-4", CyclesTest.helper("a🌵8-4", "ξ.ρ.x.plus"));
        MatcherAssert.assertThat(
            "a helper reaching only a helper that reaches nothing back is no cycle, but it was",
            new Cycles(helpers).names(),
            Matchers.empty()
        );
    }

    private static Xnav helper(final String name, final String base) {
        return new Xnav(
            String.format(
                "<o name='%s'><o base='∅' name='ρ'/><o base='∅' name='i'/><o base='%s' name='φ'><o as='α0' base='ξ.i'/></o></o>",
                name, base
            )
        ).element("o");
    }
}
