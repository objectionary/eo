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

import java.net.MalformedURLException;
import java.net.URL;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link OyRemote}.
 *
 * @since 1.0
 */
final class OyRemoteTest {

    @Test
    void buildsCorrectUrl() throws Exception {
        MatcherAssert.assertThat(
            new OyRemote.UrlOy(
                "https://raw/objectionary/home/%s/objects/%s.eo",
                "abcde"
            ).value("org.eolang.app"),
            Matchers.is(
                new URL("https://raw/objectionary/home/abcde/objects/org/eolang/app.eo")
            )
        );
    }

    @Test
    void throwsExceptionOnInvalidUrl() {
        Assertions.assertThrows(
            MalformedURLException.class,
            () -> new OyRemote.UrlOy(
                "hts:raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo",
                "abcde"
            ).value("org.eolang.app")
        );
    }
}
