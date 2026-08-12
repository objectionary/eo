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
import org.junit.jupiter.api.Timeout;

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
                ).rows(),
                Collections.emptyMap()
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
                ).rows(),
                Collections.emptyMap()
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
                ).rows(),
                Collections.emptyMap()
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
                ).rows(),
                Collections.emptyMap()
            ).attribute("Φ.jar", "lid"),
            Matchers.equalTo("Φ.jar.lid")
        );
    }

    @Test
    void findsAttributeBehindDelegation() {
        MatcherAssert.assertThat(
            "what stands behind the decoratee answers for a name the object does not bind",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        String.join(
                            "",
                            "<provides>",
                            "<type id='Φ.jar' complete='false'>",
                            "<attr name='φ' type='Φ.jar.φ'/></type>",
                            "<type id='Φ.base' complete='true'>",
                            "<attr name='lid' type='Φ.base.lid'/></type>",
                            "</provides>"
                        )
                    ),
                    Collections.emptyMap()
                ).rows(),
                Collections.singletonMap("Φ.jar.φ", "Φ.base")
            ).attribute("Φ.jar", "lid"),
            Matchers.equalTo("Φ.base.lid")
        );
    }

    @Test
    void seesWholeOfTypeThroughItsDecoratee() {
        MatcherAssert.assertThat(
            "an object that hands its answers to one that was seen whole was seen whole too",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        String.join(
                            "",
                            "<provides>",
                            "<type id='Φ.jar' complete='false'>",
                            "<attr name='φ' type='Φ.jar.φ'/></type>",
                            "<type id='Φ.base' complete='true'/>",
                            "</provides>"
                        )
                    ),
                    Collections.emptyMap()
                ).rows(),
                Collections.singletonMap("Φ.jar.φ", "Φ.base")
            ).complete("Φ.jar"),
            Matchers.is(true)
        );
    }

    @Test
    @Timeout(10L)
    void walksOutOfDelegationThatComesBack() {
        MatcherAssert.assertThat(
            "an object whose decoratee leads back to it must be walked once and left alone",
            new Provided(
                new Ungrouped(
                    new XMLDocument(
                        String.join(
                            "",
                            "<provides><type id='Φ.jar' complete='false'>",
                            "<attr name='φ' type='Φ.jar.φ'/>",
                            "</type></provides>"
                        )
                    ),
                    Collections.emptyMap()
                ).rows(),
                Collections.singletonMap("Φ.jar.φ", "Φ.jar")
            ).complete("Φ.jar"),
            Matchers.is(false)
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
                ).rows(),
                Collections.emptyMap()
            ).attribute("Φ.jar", "lid"),
            Matchers.emptyString()
        );
    }
}
