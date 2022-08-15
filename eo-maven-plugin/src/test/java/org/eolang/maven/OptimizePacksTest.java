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
package org.eolang.maven;

import java.util.Collection;
import java.util.LinkedList;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.parser.CheckPack;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for packs.
 *
 * @since 1.0
 */
public final class OptimizePacksTest {

    @ParameterizedTest
    @MethodSource("yamlPacks")
    public void testPacks(final String pack) throws Exception {
        MatcherAssert.assertThat(
            new CheckPack(
                new TextOf(
                    new ResourceOf(
                        String.format("org/eolang/maven/packs/%s", pack)
                    )
                ).asString()
            ).failures(),
            Matchers.empty()
        );
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Collection<String> yamlPacks() {
        return OptimizePacksTest.yamls("org/eolang/maven/packs/", "");
    }

    private static Collection<String> yamls(final String path,
        final String prefix) {
        final Collection<String> out = new LinkedList<>();
        final String[] paths = new UncheckedText(
            new TextOf(new ResourceOf(path))
        ).asString().split("\n");
        for (final String sub : paths) {
            if (sub.endsWith(".yaml")) {
                out.add(String.format("%s%s", prefix, sub));
            } else {
                out.addAll(
                    OptimizePacksTest.yamls(
                        String.format("%s%s/", path, sub),
                        String.format("%s/", sub)
                    )
                );
            }
        }
        return out;
    }

}
