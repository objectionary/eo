/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.TjSmart;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Provides}.
 * @since 0.67.0
 */
final class ProvidesTest {

    @Test
    void listsEveryAttributeOfFormation() {
        MatcherAssert.assertThat(
            "a formation must give a row to every attribute it binds, but it doesnt",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.kettle' name='kettle'>",
                        "<o loc='Φ.kettle.steam' name='steam'/>",
                        "<o loc='Φ.kettle.whistle' name='whistle'/>",
                        "</o></object>"
                    )
                )
            ).rows().select(row -> row.exists("owner")),
            Matchers.hasSize(2)
        );
    }

    @Test
    void recordsTypeOfAttribute() {
        MatcherAssert.assertThat(
            "the row of an attribute must carry the type of the object bound to it",
            new TjSmart(
                new Provides(
                    new XMLDocument(
                        String.join(
                            "",
                            "<object><o loc='Φ.kettle' name='kettle'>",
                            "<o loc='Φ.kettle.steam' name='steam'/>",
                            "</o></object>"
                        )
                    )
                ).rows()
            ).getById("Φ.kettle steam").get("type"),
            Matchers.equalTo("Φ.kettle.steam")
        );
    }

    @Test
    void marksFormationComplete() {
        MatcherAssert.assertThat(
            "a formation we have seen entirely must be complete, but it isnt",
            new TjSmart(
                new Provides(
                    new XMLDocument("<object><o loc='Φ.lid' name='lid'/></object>")
                ).rows()
            ).getById("Φ.lid").get("complete"),
            Matchers.equalTo("true")
        );
    }

    @Test
    void marksVoidAttribute() {
        MatcherAssert.assertThat(
            "a void attribute must be marked as such, but it isnt",
            new TjSmart(
                new Provides(
                    new XMLDocument(
                        String.join(
                            "",
                            "<object><o loc='Φ.pipe' name='pipe'>",
                            "<o base='∅' loc='Φ.pipe.width' name='width'/>",
                            "</o></object>"
                        )
                    )
                ).rows()
            ).getById("Φ.pipe width").get("void"),
            Matchers.equalTo("true")
        );
    }

    @Test
    void leavesAtomIncomplete() {
        MatcherAssert.assertThat(
            "an atom hides its body in Java, so its row cannot be complete, but it is",
            new TjSmart(
                new Provides(
                    new XMLDocument(
                        String.join(
                            "",
                            "<object><o loc='Φ.tick' name='tick'>",
                            "<o atom='Φ.number' loc='Φ.tick.λ' name='λ'/>",
                            "</o></object>"
                        )
                    )
                ).rows()
            ).getById("Φ.tick").get("complete"),
            Matchers.equalTo("false")
        );
    }

    @Test
    void skipsApplications() {
        MatcherAssert.assertThat(
            "an application provides nothing of its own, so it must have no row, but it has",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.lamp' name='lamp'>",
                        "<o base='ξ.bulb' loc='Φ.lamp.φ' name='φ'/>",
                        "</o></object>"
                    )
                )
            ).rows().select(row -> "Φ.lamp.φ".equals(row.get("id"))),
            Matchers.empty()
        );
    }

    @Test
    void skipsData() {
        MatcherAssert.assertThat(
            "bytes are not a formation, so they must have no row, but they have",
            new Provides(
                new XMLDocument(
                    String.join(
                        "",
                        "<object><o loc='Φ.two' name='two'>",
                        "<o base='Φ.bytes' loc='Φ.two.φ' name='φ'>",
                        "<o as='α0' loc='Φ.two.φ.α0'>02-</o>",
                        "</o></o></object>"
                    )
                )
            ).rows().select(row -> "Φ.two.φ.α0".equals(row.get("id"))),
            Matchers.empty()
        );
    }
}
