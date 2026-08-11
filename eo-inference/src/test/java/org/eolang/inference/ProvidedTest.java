/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.xml.XMLDocument;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Provided}.
 *
 * <p>What the checker understands about a program is checked by the packs of
 * the Maven plugin, which read EO source. These are the three questions the
 * loop asks of the provides table, which know nothing about EO, so they are
 * asked here directly.</p>
 *
 * @since 0.68.0
 */
final class ProvidedTest {

    @Test
    void keepsVoidsInOrderTheyWereDeclared() {
        MatcherAssert.assertThat(
            "an argument fills a void by its place, so the voids must keep their order",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        String.join(
                            "",
                            "<provides><type id='Φ.jar'>",
                            "<attr name='lid' void='true' type='Φ.jar.lid'/>",
                            "<attr name='spout' void='true' type='Φ.jar.spout'/>",
                            "</type></provides>"
                        )
                    ),
                    Collections.emptyMap()
                ).rows()
            ).voids("Φ.jar"),
            Matchers.contains("Φ.jar.lid", "Φ.jar.spout")
        );
    }

    @Test
    void countsOnlyVoidsAmongAttributes() {
        MatcherAssert.assertThat(
            "an attribute that is already bound is no place for an argument to land in",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        String.join(
                            "",
                            "<provides><type id='Φ.jar'>",
                            "<attr name='honey' type='Φ.jar.honey'/>",
                            "<attr name='lid' void='true' type='Φ.jar.lid'/>",
                            "</type></provides>"
                        )
                    ),
                    Collections.emptyMap()
                ).rows()
            ).voids("Φ.jar"),
            Matchers.contains("Φ.jar.lid")
        );
    }

    @Test
    void doubtsTypeWhenOneOfItsNamesHidesSomething() {
        MatcherAssert.assertThat(
            "an argument put into a copy of an atom is as unknown as the atom is",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        String.join(
                            "",
                            "<provides>",
                            "<type id='Φ.jar' complete='true'/>",
                            "<type id='Φ.tin' complete='false'/>",
                            "</provides>"
                        )
                    ),
                    Collections.singletonMap("Φ.tin", "Φ.jar")
                ).rows()
            ).complete("Φ.jar"),
            Matchers.is(false)
        );
    }

    @Test
    void findsAttributeBesideTypeInItsPackage() {
        MatcherAssert.assertThat(
            "an attribute nobody binds falls through to the object of that name in the package",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        String.join(
                            "",
                            "<provides>",
                            "<type id='Φ.jar' complete='true'/>",
                            "<type id='Φ.jar.lid' complete='true'/>",
                            "</provides>"
                        )
                    ),
                    Collections.emptyMap()
                ).rows()
            ).attribute("Φ.jar", "lid"),
            Matchers.equalTo("Φ.jar.lid")
        );
    }

    @Test
    void saysNothingAboutAttributeTypeDoesNotHave() {
        MatcherAssert.assertThat(
            "a type that has no such attribute must answer with nothing at all",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        "<provides><type id='Φ.jar' complete='true'/></provides>"
                    ),
                    Collections.emptyMap()
                ).rows()
            ).attribute("Φ.jar", "lid"),
            Matchers.emptyString()
        );
    }
}
