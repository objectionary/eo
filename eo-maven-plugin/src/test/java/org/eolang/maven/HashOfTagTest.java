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

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link OyRemote}.
 * @since 0.26
 */
final class HashOfTagTest {

    @BeforeEach
    void weAreOnline() throws IOException {
        try {
            Assumptions.assumeTrue(
                InetAddress.getByName("home.objectionary.com").isReachable(1000)
            );
        } catch (final UnknownHostException ex) {
            Assumptions.assumeTrue(false);
        }
    }

    @Test
    void testCommitHashTag() throws IOException {
        final String hash = new HashOfTag("0.26.0").hash();
        MatcherAssert.assertThat(
            hash,
            Matchers.equalTo("e0b783692ef749bb184244acb2401f551388a328")
        );
    }

    @Test
    void testCommitHashOldTag() throws IOException {
        final String hash = new HashOfTag("0.23.19").hash();
        MatcherAssert.assertThat(
            hash,
            Matchers.equalTo("4b19944d86058e3c81e558340a3a13bc335a2b48")
        );
    }

    @Test
    void testCommitHashException() {
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new HashOfTag("nonsense").hash()
        );
    }
}
