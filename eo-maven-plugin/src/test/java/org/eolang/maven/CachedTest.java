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

import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Tests for Cached.
 * @since 1.0
 */
public final class CachedTest {
    @Test
    public void testContentOfCachedFile(@TempDir final Path temp) throws Exception {
        final String content = String.join(
            "\n",
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<program>",
            "</program>"
        );
        new Cached("1.0.0", "org.eolang.txt.text.xmir", temp.resolve("parsed"))
            .save(content);
        MatcherAssert.assertThat(
            new Cached("1.0.0", "org.eolang.txt.text.xmir", temp.resolve("parsed")).content(),
            Matchers.equalTo(content)
        );
    }

    @Test
    public void testCachedObjectExists(@TempDir final Path temp) throws Exception {
        new Cached("1.0.0", "org.eolang.txt.text.xmir", temp.resolve("parsed"))
            .save(
                String.join(
                    "\n",
                    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                    "<program>",
                    "</program>"
            )
        );
        MatcherAssert.assertThat(
            new Cached("1.0.0", "org.eolang.txt.text.xmir", temp.resolve("parsed")).exists(),
            Matchers.is(true)
        );
    }

    @ValueSource(strings = {"1.0.0", "0.0.1", "abc", "a.b.c.0"})
    @ParameterizedTest
    public void testMeaningfulVersion(final String ver) {
        MatcherAssert.assertThat(
            ParseMojo.versioned(ver),
            Matchers.is(true)
        );
    }

    @ValueSource(strings = {"0.0.0", "*.*.*", "", "   "})
    @ParameterizedTest
    public void testNoVersion(final String ver) {
        MatcherAssert.assertThat(
            ParseMojo.versioned(ver),
            Matchers.is(false)
        );
    }
}
