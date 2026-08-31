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
    void findsNoLocationsInAnEmptyFormation() throws Exception {
        MatcherAssert.assertThat(
            "an empty formation has no body and is never dispatched itself, so it must have no locations",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(System.lineSeparator(), "[] > foo", "")
                ).parsed()
            ),
            Matchers.iterableWithSize(0)
        );
    }

    @Test
    void findsNoLocationsInASourceThatDoesNotParse() throws Exception {
        MatcherAssert.assertThat(
            "a parser error carries a line and a position and no locator, so it cannot be a location",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(System.lineSeparator(), "[] > x", "  TRUE > t", "")
                ).parsed()
            ),
            Matchers.iterableWithSize(0)
        );
    }

    @Test
    void excludesLocationOfAnAnonymousFormation() throws Exception {
        MatcherAssert.assertThat(
            "an anonymous formation becomes a nested class the transpiler never instruments, so its line must not be counted",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "+package examples",
                        "",
                        "[] > x",
                        "  bool > @",
                        "    []",
                        "      ? >> left",
                        "      ? >> right",
                        "      right > @",
                        ""
                    )
                ).parsed()
            ),
            Matchers.not(Matchers.hasItem(Matchers.containsString(":5:")))
        );
    }

    @Test
    void excludesLocationOfAFilesRootObject() throws Exception {
        MatcherAssert.assertThat(
            "a file's root object is constructed once from Java and never dispatched through PhCoverage, but its own location was still found",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[x] > foo",
                        "  x > @",
                        ""
                    )
                ).parsed()
            ),
            Matchers.everyItem(Matchers.not(Matchers.matchesPattern("^[^:]+:1:\\d+$")))
        );
    }

    @Test
    void excludesLocationsOfAnAtomAttribute() throws Exception {
        MatcherAssert.assertThat(
            "an atom attribute has no body in .eo, so neither it nor its lambda marker may be counted, but a location was still found",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > foo",
                        "  [] > bar /Q.bytes",
                        ""
                    )
                ).parsed()
            ),
            Matchers.iterableWithSize(0)
        );
    }

    @Test
    void excludesLocationsOfTheVoidsOfAnAtomAttribute() throws Exception {
        MatcherAssert.assertThat(
            "the voids an atom attribute declares are declarations of a Java body, so they must not be counted either",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > foo",
                        "  42 > x",
                        "  [] > bar /Q.bytes",
                        "    ? > offset /Q.number",
                        ""
                    )
                ).parsed()
            ),
            Matchers.everyItem(Matchers.not(Matchers.matchesPattern("^[^:]+:[34]:\\d+$")))
        );
    }

    @Test
    void keepsLocationsOfAnObjectHoldingAnAtomAttribute() throws Exception {
        MatcherAssert.assertThat(
            "only the atom attribute itself is left out, while the ordinary attributes beside it are still counted",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > foo",
                        "  42 > x",
                        "  [] > bar /Q.bytes",
                        ""
                    )
                ).parsed()
            ),
            Matchers.hasItem(Matchers.matchesPattern("^[^:]+:2:\\d+$"))
        );
    }

    @Test
    void excludesLocationOfAWholeAtomClass() throws Exception {
        MatcherAssert.assertThat(
            "a class that is itself an atom never gets a PhCoverage hit for its own line, but the location was still found",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > foo /Q.bytes",
                        "",
                        "  true.eq true ++> works",
                        ""
                    )
                ).parsed()
            ),
            Matchers.everyItem(Matchers.not(Matchers.matchesPattern("^[^:]+:1:\\d+$")))
        );
    }

    @Test
    void excludesLocationOfAThrowingCase() throws Exception {
        MatcherAssert.assertThat(
            "a throwing test never gets a PhCoverage hit for its own body, but its locations were still found",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "+package foo.x",
                        "",
                        "[] > main",
                        "",
                        "  --> stops-on-dispatching-on-a-number",
                        "    42.plus 1 > @",
                        ""
                    )
                ).parsed()
            ),
            Matchers.everyItem(Matchers.not(Matchers.containsString(".-")))
        );
    }

    @Test
    void excludesLocationsOfVoidAttributesDeclaredWithTheShorthand() throws Exception {
        MatcherAssert.assertThat(
            "a `? >> name` void attribute has no dataizable body, so its declaration line must not be counted",
            new CoverageManifest().locations(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "bool > x",
                        "  []",
                        "    ? >> left",
                        "    ? >> right",
                        "    right > @",
                        ""
                    )
                ).parsed()
            ),
            Matchers.everyItem(Matchers.not(Matchers.matchesPattern("^[^:]+:[34]:\\d+$")))
        );
    }

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
