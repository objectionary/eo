/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link OyRemote}.
 * @since 0.26
 */
public class HashGettingTest {
    @Test
    public void testCommitHash() throws Exception {
        final String format = "%s%s%s";
        final String domain = "https://raw.githubusercontent.com/objectionary/home/";
        final String suffix = "/objects/%s.eo";
        OyRemote instance = new OyRemote("0.23.19").resolve();
        MatcherAssert.assertThat(
            instance.getTemplate(),
            Matchers.equalTo(
                String.format(
                    format,
                    domain,
                    "4b19944d86058e3c81e558340a3a13bc335a2b48",
                    suffix
                )
            )
        );
        instance = new OyRemote("0.26.0").resolve();
        MatcherAssert.assertThat(
            instance.getTemplate(),
            Matchers.equalTo(
                String.format(
                    format,
                    domain,
                    "e0b783692ef749bb184244acb2401f551388a328",
                    suffix
                )
            )
        );
    }
}
