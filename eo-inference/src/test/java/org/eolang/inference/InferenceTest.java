/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import java.io.IOException;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Inference}.
 * @since 0.67.0
 */
final class InferenceTest {

    @Test
    void splitsCompositeBase() throws IOException {
        MatcherAssert.assertThat(
            "a chain of dispatches must become one object per step, but it didnt",
            new Inference(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[x] > inc",
                        "  x.next.foo > @",
                        ""
                    )
                ).parsed()
            ).prepared(),
            XhtmlMatchers.hasXPath("//o[@base='.foo']/o[@base='.next']/o[@base='ξ.x']")
        );
    }

    @Test
    void keepsReferenceWhole() throws IOException {
        MatcherAssert.assertThat(
            "a reference takes no attribute from anything, so it must stay whole, but it didnt",
            new Inference(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > tap",
                        "  water > @",
                        "  [] > water",
                        ""
                    )
                ).parsed()
            ).prepared(),
            XhtmlMatchers.hasXPath("//o[@base='ξ.water']")
        );
    }

    @Test
    void locatesObjectsBornFromSplitting() throws IOException {
        MatcherAssert.assertThat(
            "the receiver of a new dispatch must get a locator of its own, but it didnt",
            new Inference(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[x] > inc",
                        "  x.next.foo > @",
                        ""
                    )
                ).parsed()
            ).prepared(),
            XhtmlMatchers.hasXPath("//o[@base='ξ.x' and @loc='Φ.inc.φ.ρ.ρ']")
        );
    }

    @Test
    void buildsProvidesTableForTheWholeProgram() throws IOException {
        MatcherAssert.assertThat(
            "the innermost formation must be known to have nothing, but it isnt",
            new Inference(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > app",
                        "  inc t > @",
                        "  [] > t",
                        "    [] > next",
                        "  [x] > inc",
                        "    x.next.foo > @",
                        ""
                    )
                ).parsed()
            ).provides(),
            XhtmlMatchers.hasXPath(
                "/provides/type[@id='Φ.app.t.next' and @complete='true' and not(attr)]"
            )
        );
    }
}
