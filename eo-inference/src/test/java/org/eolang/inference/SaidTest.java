/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Said}.
 *
 * @since 0.73.0
 */
final class SaidTest {

    @Test
    void saysWhatThePairsSay() {
        MatcherAssert.assertThat(
            "the pairs handed over must be the ones the table says, but they werent",
            new Said(new Pairs(new XMLDocument("<links/>"))).with(
                Collections.singletonMap("Φ.oak", "Φ.tree"),
                Collections.emptyMap()
            ).all(),
            Matchers.hasEntry("Φ.oak", "Φ.tree")
        );
    }

    @Test
    void readsTheBindsTheOtherWayRound() {
        MatcherAssert.assertThat(
            "the void must name what went into it, but it didnt",
            new Said(new Pairs(new XMLDocument("<links/>"))).with(
                Collections.singletonMap("Φ.oak", "Φ.tree"),
                Collections.singletonMap(
                    "Φ.oak", Collections.singletonMap("Φ.tree.seed", "Φ.acorn")
                )
            ).puts(),
            Matchers.hasEntry(Matchers.is("Φ.tree.seed"), Matchers.contains("Φ.acorn"))
        );
    }

    @Test
    void gathersWhatTwoCopiesPutIntoOneVoid() {
        final Map<String, String> pairs = new LinkedHashMap<>(0);
        pairs.put("Φ.oak", "Φ.tree");
        pairs.put("Φ.elm", "Φ.tree");
        final Map<String, Map<String, String>> binds = new LinkedHashMap<>(0);
        binds.put("Φ.oak", Collections.singletonMap("Φ.tree.seed", "Φ.acorn"));
        binds.put("Φ.elm", Collections.singletonMap("Φ.tree.seed", "Φ.samara"));
        MatcherAssert.assertThat(
            "the void must name both the objects put into it, but it didnt",
            new Said(new Pairs(new XMLDocument("<links/>")))
                .with(pairs, binds)
                .puts()
                .get("Φ.tree.seed"),
            Matchers.contains("Φ.acorn", "Φ.samara")
        );
    }

    @Test
    void dropsThePairARowOfAnotherFormOverwrites() {
        MatcherAssert.assertThat(
            "the row the table gave a form of its own must beat the pair, but it didnt",
            new Said(
                new Pairs(new XMLDocument("<links><type id='Φ.oak'><data/></type></links>"))
            ).with(
                Collections.singletonMap("Φ.oak", "Φ.tree"),
                Collections.emptyMap()
            ).all(),
            Matchers.not(Matchers.hasKey("Φ.oak"))
        );
    }

    @Test
    void keepsThePairARowOfItsOwnKindDoesNot() {
        MatcherAssert.assertThat(
            "the pair the table wrote itself must survive the pass, but it didnt",
            new Said(
                new Pairs(
                    new XMLDocument(
                        "<links><type id='Φ.oak'><ref loc='Φ.tree'/></type></links>"
                    )
                )
            ).with(
                Collections.singletonMap("Φ.oak", "Φ.plant"),
                Collections.emptyMap()
            ).all(),
            Matchers.hasEntry("Φ.oak", "Φ.plant")
        );
    }

    @Test
    void keepsTheFormsNoPassCanWrite() {
        MatcherAssert.assertThat(
            "the form the table gave a row must survive the pass, but it didnt",
            new Said(
                new Pairs(
                    new XMLDocument("<links><type id='Φ.oak'><terminator/></type></links>")
                )
            ).with(
                Collections.singletonMap("Φ.elm", "Φ.tree"),
                Collections.emptyMap()
            ).forms(),
            Matchers.hasEntry("Φ.oak", "terminator")
        );
    }

    @Test
    void keepsWhatTheTableAnswersByItself() {
        MatcherAssert.assertThat(
            "the object the table answers by itself must survive the pass, but it didnt",
            new Said(
                new Pairs(new XMLDocument("<links><type id='Φ.oak'><data/></type></links>"))
            ).with(
                Collections.singletonMap("Φ.elm", "Φ.tree"),
                Collections.emptyMap()
            ).certain(),
            Matchers.contains("Φ.oak")
        );
    }
}
