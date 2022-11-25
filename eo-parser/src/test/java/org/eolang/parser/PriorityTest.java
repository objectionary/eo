/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import com.jcabi.xml.XMLDocument;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.stream.Stream;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * We expect that generated XML will contain the method '.plus' with all required components.
 * Example. For the program:
 *
 * <pre>{@code
 *  [] > x
 *   1.times 2 (1.plus other.value)
 * }
 * </pre>
 *
 * Both components - application and method (`other.value`) have to be included to the plus object:
 * <pre>{@code
 *  &lt;o base=".plus" line="2" method="" pos="14">
 *    &lt;o base="other" line="2" pos="20"/>
 *    &lt;o base=".value" line="2" method="" pos="25"/>
 *  &lt;/o>
 * }
 * </pre>
 *
 * The important part here is that '.value' have to be placed exactly inside '.plus'. Earlier
 * it was differently (and wrong):
 * <pre>{@code
 *  &lt;o base=".plus" line="2" method="" pos="14">
 *    &lt;o base="other" line="2" pos="20"/>
 *  &lt;/o>
 *  &lt;o base=".value" line="2" method="" pos="25"/>
 * }
 * </pre>
 * That caused lots of problems.
 *
 * @since 0.28.12
 */
class PriorityTest {

    @ParameterizedTest
    @MethodSource("plusTestCases")
    void checksPriorityForPlusObject(
        final String program,
        final String... bases
    ) throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final Syntax syntax = new Syntax("", new InputOf(program), new OutputTo(baos));
        syntax.parse();
        for (final String base : bases) {
            MatcherAssert.assertThat(
                new XMLDocument(baos.toByteArray())
                    .nodes("//o[@base='.plus']")
                    .get(0).nodes(String.format("//o[@base='%s']", base)),
                Matchers.not(Matchers.empty())
            );
        }
    }

    static Stream<Arguments> plusTestCases() {
        return Stream.of(
            Arguments.of(
                "[] > x\n  1.times 2 (1.plus other.value)",
                new String[]{"other", ".value"}
            ),
            Arguments.of(
                "[] > sum\n  ^.a.plus ^.b > @\n",
                new String[]{"^", ".b"}
            )
        );
    }
}
