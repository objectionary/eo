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

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.stream.Collectors;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Test case for all .xsl files, which are specified in .yaml files
 * in {@code src/test/xsl} directories.
 *
 * @since 0.21
 */
public final class XslsTest {

    /**
     * Where is the home of this module.
     */
    private static final Path HOME = Paths.get(
        System.getProperty("basedir", "eo-parser")
    );

    @ParameterizedTest
    @MethodSource("yamlPacks")
    public void testPacks(final Path path) throws Exception {
        MatcherAssert.assertThat(
            new CheckXSL(
                XslsTest.HOME.resolve("src/main/resources"),
                new TextOf(path).asString()
            ).isValid(),
            Matchers.is(true)
        );
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Collection<Path> yamlPacks() throws IOException {
        final Collection<Path> list = Files.walk(XslsTest.HOME)
            .filter(
                file -> file.toString().contains(
                    Paths.get("src").resolve("/test/xsl/").toString()
                )
            )
            .filter(file -> file.toString().endsWith(".yml"))
            .filter(file -> !file.toFile().isDirectory())
            .collect(Collectors.toList());
        MatcherAssert.assertThat(
            String.format(
                "Can't find any .yml files in '%s' directory",
                XslsTest.HOME
            ),
            list,
            Matchers.not(Matchers.emptyIterable())
        );
        return list;
    }
}
