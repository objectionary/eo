/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.Collection;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link CoverageManifest}.
 * @since 0.75.0
 */
final class CoverageManifestTest {

    @Test
    void findsLocationsOfASimpleObject() throws Exception {
        MatcherAssert.assertThat(
            "a formation with a body must have at least one instrumented location, but none were found",
            this.simpleObject(),
            Matchers.not(Matchers.empty())
        );
    }

    @Test
    void findsLocationsShapedAsLocLinePos() throws Exception {
        MatcherAssert.assertThat(
            "every found location must look like loc:line:pos, but at least one didnt",
            this.simpleObject(),
            Matchers.everyItem(Matchers.matchesPattern("^[^:]+:\\d+:\\d+$"))
        );
    }

    @Test
    void findsExactlyOneLocationInAnEmptyFormation() throws Exception {
        MatcherAssert.assertThat(
            "an empty formation with no body still has itself to instrument, but the count was off",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(System.lineSeparator(), "[] > foo", "")
                ).parsed()
            ),
            Matchers.iterableWithSize(1)
        );
    }

    /**
     * Locations found in a small formation with one attribute.
     * @return The locations
     * @throws Exception If parsing or deriving them fails
     */
    private Collection<String> simpleObject() throws Exception {
        return new CoverageManifest().locations(
            new EoSyntax(
                String.join(
                    System.lineSeparator(),
                    "[] > foo",
                    "  42 > x",
                    ""
                )
            ).parsed()
        );
    }
}
