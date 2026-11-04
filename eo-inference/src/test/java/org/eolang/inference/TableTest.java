/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Table}.
 *
 * @since 0.76.0
 */
final class TableTest {

    @Test
    void countsALabelInTheCopiesOfEverySetThatHoldsIt() {
        MatcherAssert.assertThat(
            "a label must be counted in every set that holds it, but it wasnt",
            new Table(
                new Xnav(
                    "<answers><o loc='Φ.kiln.x' copies='232'>",
                    "<set copies='119'><terminator/><is loc='Φ.bool'/></set>",
                    "<set copies='112'><is loc='Φ.bool'/></set>",
                    "<set copies='1'><unknown kind='loop'/></set></o></answers>"
                )
            ).said("Φ.kiln.x").get("Φ.bool"),
            Matchers.equalTo(231)
        );
    }

    @Test
    void addsUpTheCopiesOfEverySet() {
        MatcherAssert.assertThat(
            "the copies of a place must be those of all its sets, but they werent",
            new Table(
                new Xnav(
                    "<answers><o loc='Φ.kiln.y' copies='9'>",
                    "<set copies='5'><data/></set><set copies='4'><unknown kind='study'/></set></o></answers>"
                )
            ).copies("Φ.kiln.y"),
            Matchers.equalTo(9)
        );
    }

    @Test
    void readsTheLabelsOfARowWithOneSet() {
        MatcherAssert.assertThat(
            "the labels right under a row must be read as its one set, but they werent",
            new Table(
                new Xnav(
                    "<answers><o loc='Φ.kiln' copies='3'>",
                    "<is loc='Φ.kiln'/><terminator/></o></answers>"
                )
            ).said("Φ.kiln").keySet(),
            Matchers.contains("Φ.kiln", "T")
        );
    }

    @Test
    void leavesOutWhatAFormationReducesTo() {
        MatcherAssert.assertThat(
            "what a formation reduces to must stay out of what it says, but it didnt",
            new Table(
                new Xnav(
                    "<answers><o loc='Φ.kiln.fire' copies='104'>",
                    "<is loc='Φ.kiln.fire'/>",
                    "<reduced copies='104'><is loc='Φ.number'/></reduced></o></answers>"
                )
            ).said("Φ.kiln.fire").keySet(),
            Matchers.contains("Φ.kiln.fire")
        );
    }

    @Test
    void readsAnUnknownByItsKind() {
        MatcherAssert.assertThat(
            "an unknown must be read as the mark of its kind, but it wasnt",
            new Table(
                new Xnav("<answers><o loc='Φ.kiln.t' copies='3'><unknown kind='study'/></o></answers>")
            ).said("Φ.kiln.t").keySet(),
            Matchers.contains("?unknown-study")
        );
    }

    @Test
    void readsAVoidJavaFillsAsTheAtomsVoid() {
        MatcherAssert.assertThat(
            "a void Java fills must be read as a void an atom fills, but it wasnt",
            new Table(
                new Xnav("<answers><o loc='Φ.kiln.z' copies='2'><unknown kind='java'/></o></answers>")
            ).said("Φ.kiln.z").keySet(),
            Matchers.contains("?unknown-java")
        );
    }

    @Test
    void refusesAnUnknownOfAKindItDoesNotKnow() {
        MatcherAssert.assertThat(
            "an unknown of a kind the report does not know must stop it and be named, but it wasnt",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new Table(
                    new Xnav("<answers><o loc='Φ.kiln.q' copies='5'><unknown kind='lost'/></o></answers>")
                ).said("Φ.kiln.q"),
                "an unknown of a kind the report does not know must stop it"
            ).getMessage(),
            Matchers.containsString("lost")
        );
    }

    @Test
    void refusesAnyFailure() {
        MatcherAssert.assertThat(
            "a failure must stop the report and be named, but it wasnt",
            Assertions.assertThrows(
                IllegalArgumentException.class,
                () -> new Table(
                    new Xnav(
                        "<answers><o loc='Φ.kiln.ρ' copies='6'>",
                        "<fails name='ρ' by='java'/><is loc='Φ.string'/></o></answers>"
                    )
                ).said("Φ.kiln.ρ"),
                "a failure must stop the report"
            ).getMessage(),
            Matchers.containsString("<fails>")
        );
    }

    @Test
    void countsTheCopiesThatSayAnyOfTheLabels() {
        MatcherAssert.assertThat(
            "only the sets that hold one of the labels must count, but others did",
            new Table(
                new Xnav(
                    "<answers><o loc='Φ.kiln.v' copies='17'>",
                    "<set copies='10'><is loc='Φ.a🌵1-1'/></set>",
                    "<set copies='4'><is loc='Φ.a🌵1-1'/><is loc='Φ.a🌵2-1'/></set>",
                    "<set copies='3'><terminator/></set></o></answers>"
                )
            ).joined("Φ.kiln.v", Arrays.asList("Φ.a🌵1-1", "Φ.a🌵2-1")),
            Matchers.equalTo(14)
        );
    }

    @Test
    void readsHowManyCopiesOfAFormationAreStudyCopies() {
        MatcherAssert.assertThat(
            "the study copies of a formation must be read off its row, but they werent",
            new Table(
                new Xnav(
                    "<answers><o loc='Φ.kiln.oven' copies='6' study='4'>",
                    "<is loc='Φ.kiln.oven'/></o></answers>"
                )
            ).study("Φ.kiln.oven"),
            Matchers.equalTo(4)
        );
    }

    @Test
    void countsNoStudyCopiesOfARowThatSaysNone() {
        MatcherAssert.assertThat(
            "a row that says no study copies must have none, but it had some",
            new Table(
                new Xnav("<answers><o loc='Φ.kiln.w' copies='8'><data/></o></answers>")
            ).study("Φ.kiln.w"),
            Matchers.equalTo(0)
        );
    }

    @Test
    void countsNoCopiesOfAPlaceWithNoRow() {
        MatcherAssert.assertThat(
            "a place the table has no row for must have no copies, but it had some",
            new Table(new Xnav("<answers/>")).copies("Φ.kiln.u"),
            Matchers.equalTo(0)
        );
    }
}
