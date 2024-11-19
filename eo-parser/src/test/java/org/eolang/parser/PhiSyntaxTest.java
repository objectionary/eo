/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import java.io.IOException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.xembly.Directives;

/**
 * Test cases for {@link PhiSyntax}.
 *
 * @since 0.35.0
 */
final class PhiSyntaxTest {
    @ParameterizedTest
    @CsvSource({
        "empty ↦ Φ.org.eolang.bytes, program structure is invalid",
        "{⟦obj ↦ ⟦⟧(x ↦ ⟦⟧)⟧}, application on formation is not supported",
        "{⟦obj ↦ ⟦a ↦ ⟦⟧.y(Δ ⤍ 00-)⟧⟧}, delta application is not supported"
    })
    void addsError(final String program, final String message) throws IOException {
        MatcherAssert.assertThat(
            String.format("Result XML must contain errors because %s", message),
            new PhiSyntax(program).parsed(),
            XhtmlMatchers.hasXPath("//errors[count(error)>0]")
        );
    }

    @Test
    void catchesDeltaToNothingBinding() {
        Assertions.assertThrows(
            ParsingException.class,
            new PhiSyntax("{ ⟦ x ↦ ⟦ Δ ⤍ ∅ ⟧ ⟧ }")::parsed,
            "Impossible binding with Δ should be caught"
        );
    }

    @Test
    void addsExtra() throws IOException {
        MatcherAssert.assertThat(
            "Result XML must contain extra object",
            new PhiSyntax(
                "test",
                () -> "{⟦obj ↦ ⟦⟧⟧}",
                new Directives().xpath("/program/objects").add("o").attr("base", "x")
            ).parsed(),
            XhtmlMatchers.hasXPath(
                "//objects/o[@base='x']"
            )
        );
    }
}
