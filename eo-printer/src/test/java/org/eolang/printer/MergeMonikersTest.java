/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.util.Objects;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for the merge-monikers stylesheet.
 * @since 0.60.0
 */
final class MergeMonikersTest {

    /** Stylesheet under test. */
    private final XML sheet;

    /**
     * New test instance.
     * @throws IOException If the stylesheet cannot be read
     */
    MergeMonikersTest() throws IOException {
        this.sheet = new XMLDocument(
            Objects.requireNonNull(
                MergeMonikersTest.class.getResourceAsStream(
                    "/org/eolang/printer/print/merge-monikers.xsl"
                )
            )
        );
    }

    @Test
    void cachesRepeatedHotFunctions() {
        MatcherAssert.assertThat(
            "The repeated merge-monikers lookups must use XSLT 3.0 memoization",
            this.sheet,
            XhtmlMatchers.hasXPaths(
                "/*[local-name()='stylesheet' and @version='3.0']",
                "/*/*[local-name()='function' and @name='eo:moniker-refs' and @cache='yes']",
                "/*/*[local-name()='function' and @name='eo:hosted-binding' and @cache='yes']"
            )
        );
    }

    @Test
    void guardsExpensiveTemplatePredicates() {
        MatcherAssert.assertThat(
            "Cheap predicates must reject nodes before hosted/applied lookups",
            this.sheet,
            XhtmlMatchers.hasXPaths(
                "/*/*[local-name()='template' and @match=\"o[starts-with(@base, $eo:xi-dot)][exists(eo:hosted-binding(.))]\"]",
                "/*/*[local-name()='template' and @match=\"o[starts-with(@base, $eo:xi-dot)][exists(o)][not(exists(@name))][exists(eo:applied-handle(.))]\"]"
            )
        );
    }

    @Test
    void sortsOnlyMultipleDispatches() {
        MatcherAssert.assertThat(
            "Dispatch ordering must sort only when at least two candidates exist",
            this.sheet,
            XhtmlMatchers.hasXPaths(
                "/*/*[local-name()='function' and @name='eo:moniker-refs']/*[local-name()='variable' and @name='dispatch']/*[local-name()='choose']/*[local-name()='when' and @test='exists($dispatches[2])']/*[local-name()='perform-sort' and @select='$dispatches']",
                "/*/*[local-name()='function' and @name='eo:moniker-refs']/*[local-name()='variable' and @name='dispatch']/*[local-name()='choose']/*[local-name()='otherwise']/*[local-name()='sequence' and @select='$dispatches']"
            )
        );
    }
}
