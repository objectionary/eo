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
 * Test case for {@link Behaved}.
 *
 * @since 0.70.0
 */
final class BehavedTest {

    @Test
    void namesATypeAfterTheOnlyThingItBinds() {
        MatcherAssert.assertThat(
            "an alias that binds nothing but a body must be what stands behind it, but it wasnt",
            new Behaved(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides>",
                        "<type id='Φ.oak'><attr name='leaf' type='Φ.oak.leaf'/></type>",
                        "<type id='Φ.alias'><attr name='φ' type='Φ.oak'/></type>",
                        "</provides>"
                    )
                ),
                Collections.emptyMap()
            ).all(),
            Matchers.hasEntry("Φ.alias", "Φ.oak")
        );
    }

    @Test
    void leavesATypeThatBindsMoreThanABodyAlone() {
        MatcherAssert.assertThat(
            "a trunk has a bark of its own, so it cannot be an oak, but it was named one",
            new Behaved(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides>",
                        "<type id='Φ.oak'><attr name='leaf' type='Φ.oak.leaf'/></type>",
                        "<type id='Φ.trunk'>",
                        "<attr name='φ' type='Φ.oak'/>",
                        "<attr name='bark' type='Φ.trunk.bark'/>",
                        "</type>",
                        "</provides>"
                    )
                ),
                Collections.emptyMap()
            ).all(),
            Matchers.not(Matchers.hasKey("Φ.trunk"))
        );
    }

    @Test
    void refusesToLandOnANameNoSourceFileCanWrite() {
        MatcherAssert.assertThat(
            "a moniker is worse than the name it replaces, but the type was reduced to one",
            new Behaved(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides>",
                        "<type id='Φ.alias'><attr name='φ' type='Φ.alias.a🌵3-2'/></type>",
                        "<type id='Φ.alias.a🌵3-2'>",
                        "<attr name='leaf' type='Φ.alias.a🌵3-2.leaf'/>",
                        "</type>",
                        "</provides>"
                    )
                ),
                Collections.emptyMap()
            ).all(),
            Matchers.not(Matchers.hasKey("Φ.alias"))
        );
    }

    @Test
    void countsNoVoidAmongTheAttributesThatKeepATypeApart() {
        MatcherAssert.assertThat(
            "a void is filled from outside and names nothing of its own, but it was counted",
            new Behaved(
                new XMLDocument(
                    String.join(
                        "",
                        "<provides>",
                        "<type id='Φ.oak'><attr name='leaf' type='Φ.oak.leaf'/></type>",
                        "<type id='Φ.grafted'>",
                        "<attr name='φ' type='Φ.oak'/>",
                        "<attr name='x' type='Φ.grafted.x' void='true'/>",
                        "</type>",
                        "</provides>"
                    )
                ),
                Collections.emptyMap()
            ).all(),
            Matchers.hasEntry("Φ.grafted", "Φ.oak")
        );
    }
}
