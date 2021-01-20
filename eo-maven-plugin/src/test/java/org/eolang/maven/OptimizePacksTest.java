/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
package org.eolang.maven;

import java.io.IOException;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.parser.Scenario;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for optimize-packs.
 *
 * @since 1.0
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
public final class OptimizePacksTest {

    @ParameterizedTest
    @MethodSource("yamlPacks")
    public void testPacks(final String pack) throws Exception {
        MatcherAssert.assertThat(
            new Scenario(
                new TextOf(
                    new ResourceOf(
                        String.format("org/eolang/maven/optimize-packs/%s", pack)
                    )
                ).asString()
            ).failures(),
            Matchers.empty()
        );
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static String[] yamlPacks() throws IOException {
        return new TextOf(
            new ResourceOf(
                "org/eolang/maven/optimize-packs/"
            )
        ).asString().split("\n");
    }

}
